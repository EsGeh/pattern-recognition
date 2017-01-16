{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module PatternRecogn.NeuronalNetworks(
	ClassificationParam,
	NetworkParams(..),

	LearningParams(..),
	DefaultLearningParams(..), defDefaultLearningParams,
	SilvaAlmeidaParams(..), defSilvaAlmeidaParams,
	RPropParams(..), defRPropParams,

	TrainingMonadT(), askIteration, askLastVals, askLastGradients,
	NetworkDimensions,
	StopCond(..), StopReason(..),
	OutputInterpretation(..), outputInterpretationMaximum, outputInterpretationSingleOutput,

	calcClassificationParams,
	classify,

	-- low level api:
	TrainingDataInternal, TrainingDataInternal_unpacked, packTrainingData, unpackTrainingData,
	TrainingState(..),
	trainNetwork,
	initialNetwork, initialNetworkWithRnd,
	adjustWeightsBatch,
	inputDim,
	randomPermutation,
	--adjustWeightsBatchWithRnd,
	--adjustWeightsOnLine,
	paramsDiff,
	internalFromTrainingData,
	internalFromBundledTrainingData,
) where

import PatternRecogn.NeuronalNetworks.TrainingMonad
import PatternRecogn.NeuronalNetworks.Types
import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils

--import qualified Data.Sequence as Seq
--import Data.Foldable as Fold
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Traversable( mapAccumL, forM )
import Data.List
import Data.Maybe
--import Control.DeepSeq


-----------------------------------------------------------------
-- high level api:
-----------------------------------------------------------------

calcClassificationParams ::
	(MonadLog m, MonadRandom m) =>
	LearningParams -> NetworkParams -> TrainingDataBundled -> m ClassificationParam
calcClassificationParams learningParams networkParams@NetworkParams{..} trainingData =
	fmap fst $
	do
		initialState <- randomPermutation
			(packTrainingData trainingDataInternal)
		trainNetwork
			networkParams
			[StopIfConverges 0.001]
			testWithTrainingData
			initialState
			(adjustWeightsBatch learningParams)
			=<<
			initialNetworkWithRnd (inputDim trainingDataInternal) dims
	where
		trainingDataInternal = internalFromBundledTrainingData outputInterpretation trainingData
		testWithTrainingData nw =
			testNW nw
				(Lina.fromRows $ map fst $ trainingDataInternal)
				(Lina.fromList $ map (outputToLabel outputInterpretation . snd) $ trainingDataInternal)
		testNW :: ClassificationParam -> Lina.Matrix -> VectorOf Label -> R
		testNW nw inputData expectedLabels =
			let
				testClasses = classify outputInterpretation nw $ inputData
			in
				calcClassificationQuality
					expectedLabels
					testClasses

classify :: OutputInterpretation -> ClassificationParam -> Matrix -> VectorOf Label
classify OutputInterpretation{..} param input =
	Lina.fromList $
	map (outputToLabel . last) $
	extendedClassification param input

extendedClassification ::
	ClassificationParam -> Matrix -> [[Vector]]
extendedClassification param input =
	map (feedForward param) $
	Lina.toRows $
	input


-----------------------------------------------------------------
-- low level api:
-----------------------------------------------------------------

trainNetwork ::
	forall m state .
	(MonadLog m) =>
	NetworkParams
	-> [StopCond]
	-> (ClassificationParam -> R)
	-> state
	-> (ClassificationParam -> TrainingMonadT state m TrainingState)
	-> ClassificationParam
	-> m (ClassificationParam, StopReason)
trainNetwork
		NetworkParams{..}
		stopConds
		testWithTrainingData
		initState
		adjustWeights
		initNW
	=
		evalStateT `flip` initState $
		iterateWithCtxtM 1 updateNW
		$
		initialTrainingState initNW
	where

		updateNW :: 
			TrainingState
			-> IterationMonadT [TrainingState] (StateT state m) (Either (ClassificationParam, StopReason) TrainingState)
		updateNW networkTrainingData@TrainingState{ nwData_weights=network } =
			do
			continue <- cond network
			case continue of
				Nothing ->
					do
						ret <- liftToTrainingMonad adjustWeights $ networkTrainingData
						return $ Right ret
				Just stop -> return $ Left (network, stop)

		cond :: ClassificationParam -> IterationMonadT [TrainingState] (StateT state m) (Maybe StopReason)
		cond x = withIterationCtxt $ \it previousVals ->
			cond' it (map nwData_weights previousVals) x
			where
				cond' it (previousVal:_) lastVal = return $
					(
						listToMaybe .
						catMaybes
					) $
					reasons
					where
						reasons :: [Maybe StopReason]
						reasons =
							map `flip` stopConds $ \stopCond ->
								case stopCond of
									StopAfterMaxIt maxIt ->
										if not $ it < maxIt then return $ StopReason_MaxIt it else Nothing
									StopIfConverges delta ->
										if (paramsDiff lastVal previousVal <= delta)
										then
											return $ StopReason_Converged delta it
										else Nothing
									StopIfQualityReached quality ->
										if (testWithTrainingData lastVal >= quality)
										then
											return $ StopReason_QualityReached quality it
										else Nothing
				cond' _ _ _ = return $ Nothing

inputDim :: TrainingDataInternal_unpacked -> Int
inputDim trainingData =
	case trainingData of
		fstEl:_ -> Lina.size $ fst $ fstEl
		_ -> 0

initialNetworkWithRnd :: MonadRandom m => Int -> NetworkDimensions -> m ClassificationParam
initialNetworkWithRnd inputSize dimensions =
	forM (networkMatrixDimensions inputSize dimensions) $
	\(rowCount, colCount) ->
		(rowCount><colCount) <$> getRandomRs (0,1)

initialNetwork inputSize dimensions =
	map (Lina.konst 0) $ networkMatrixDimensions inputSize dimensions

adjustWeightsBatch ::
	(MonadLog m, MonadRandom m) =>
	LearningParams
	-- -> [(Vector, Vector)]
	-> ClassificationParam -> TrainingMonadT TrainingDataInternal m TrainingState
adjustWeightsBatch either_learningParams weights =
	withSample $
	case either_learningParams of
		LearningParamsDefault learningParams ->
			adjustWeightsBatchDefault learningParams `flip` weights
		LearningParamsSilvaAlmeida learningParams ->
			adjustWeightsBatchSilvaAlmeida learningParams `flip` weights
		LearningParamsRProp learningParams ->
			adjustWeightsBatchRProp learningParams `flip` weights

withSample ::
	MonadRandom m =>
	(TrainingDataInternal_unpacked -> TrainingMonadT TrainingDataInternal m b) -> TrainingMonadT TrainingDataInternal m b
withSample f =
	do
		s <- getState
		let newState = rotateTrainingData 50 $ s
		-- newState <- randomPermutation $ s
		-- let newState = s
		putState $ newState
		f $ takeSample 50 s

adjustWeightsBatchSilvaAlmeida learningParams trainingData weights =
	askLastGradients >>= \(lastGradients:_) ->
	askLastStepWidths >>= \(lastStepWidths:_) ->
	lift $
	do
		let gradients :: [Matrix] = calcGradientsBatch weights trainingData
		let (newWeights, stepWidths) =
			applyCorrectionsSilvaAlmeida learningParams lastGradients lastStepWidths gradients weights
		return $
			TrainingState {
				nwData_weights = newWeights,
				nwData_gradients = gradients,
				nwData_stepWidths = stepWidths
			}

adjustWeightsBatchRProp learningParams trainingData weights =
	askLastGradients >>= \(lastGradients:_) ->
	askLastStepWidths >>= \(lastStepWidths:_) ->
	lift $
	do
		let (gradients :: [Matrix]) = calcGradientsBatch weights trainingData
		let (newWeights, stepWidths) =
			applyCorrectionsRProp learningParams lastGradients lastStepWidths gradients weights
		return $
			TrainingState {
				nwData_weights = newWeights,
				nwData_gradients = gradients,
				nwData_stepWidths = stepWidths
			}

adjustWeightsBatchDefault ::
	forall m state .
	(MonadRandom m) =>
	DefaultLearningParams
	-> [(Vector, Vector)] ->
	ClassificationParam -> TrainingMonadT state m TrainingState
adjustWeightsBatchDefault learningParams trainingData weights =
	--askLastVals >>= \lastValues ->
	askLastGradients >>= \(lastGradients:_) ->
	lift $
	do
		(gradients :: [Matrix]) <- lift $ calcGradientsBatchWithRnd weights trainingData
		let newWeights =
			applyCorrectionsDefault learningParams lastGradients gradients weights
		return $
			TrainingState {
				nwData_weights = newWeights,
				nwData_gradients = gradients,
				nwData_stepWidths = error "this error should never occur. Reason: stepWidth not used in default algorithm!"
			}

calcGradientsBatch ::
	ClassificationParam -> [(Vector, Vector)] -> [Matrix]
calcGradientsBatch weights trainingData =
	combineGradients $
	(map (gradientsFromSample `flip` weights) trainingData :: [[Matrix]])
	where
		combineGradients :: [[Matrix]] -> [Matrix]
		combineGradients gradientsForEachSample =
			foldr1 (zipWith (+)) $
			gradientsForEachSample

{-
randomSelSafe trainingData =
	if length trainingData > 50
	then random

randomSel :: MonadRandom m => R -> [a] -> m [a]
randomSel propability trainingData =
	fmap catMaybes $
	forM trainingData $ \x ->
	getRandomR (0,1) >>= \rVal -> return $
	if rVal >= propability then Just x else Nothing
	{-
	let
		l = length trainingData
	in
		getRandomRs (0, l-1) >>= \indices ->
			return $ map (trainingData !!) indices
	-}
-}

calcGradientsBatchWithRnd ::
	forall m . (MonadRandom m) =>
	ClassificationParam -> [(Vector, Vector)] -> m [Matrix]
calcGradientsBatchWithRnd weights trainingData =
	combineGradients $
	(map (gradientsFromSample `flip` weights) trainingData :: [[Matrix]])
	where
		combineGradients :: [[Matrix]] -> m [Matrix]
		combineGradients gradientsForEachSample =
			do
				randomVec <- getRandomRs (0,1)
				return $
					foldr1 (zipWith (+)) $
					map (\(ws,rnd) -> (rnd `Lina.scale`) <$> ws) $
					gradientsForEachSample `zip` randomVec

{-
adjustWeightsOnLine ::
	forall m .
	MonadLog m =>
	LearningParams
	-> TrainingDataInternal ->
	TrainingState -> m TrainingState
adjustWeightsOnLine trainingParams =
	foldr (<=<) return .
	map f
	where
		f :: (Vector, Vector) -> TrainingState -> m TrainingState
		f sample TrainingState{ nwData_weights=weights, nwData_gradients=lastGradients } =
			do
				gradients <- gradientsFromSample sample weights
				return $
					TrainingState {
						nwData_weights = applyCorrections trainingParams lastGradients gradients weights,
						nwData_gradients = gradients
					}
-}

applyCorrectionsRProp ::
	RPropParams -> [Matrix] -> [Matrix] -> [Matrix] -> [Matrix]
	-> ([Matrix], [Matrix])
applyCorrectionsRProp RPropParams{..} lastGradients lastStepWidths gradients weights =
	unzip $
	map `flip` (zip4 weights gradients lastGradients lastStepWidths) $
	\(w, gradient, lastGradient, lastStepWidth) ->
		let
			learnRates :: Matrix
			learnRates = Lina.fromLists $
				zipWith (zipWith conc)
				(Lina.toLists lastStepWidth) (Lina.toLists $ gradient * lastGradient)
				where
					conc lastStepWidthEntry signChange
						| signChange > 0 =
								min rprop_stepMax (lastStepWidthEntry * rprop_accelerateFactor)
						| signChange < 0 =
								max rprop_stepMin (lastStepWidthEntry * rprop_decelerateFactor)
						| otherwise =
								lastStepWidthEntry
		in
			(w - learnRates * (Lina.cmap signum gradient), learnRates)

applyCorrectionsSilvaAlmeida ::
	SilvaAlmeidaParams -> [Matrix] -> [Matrix] -> [Matrix] -> [Matrix]
	-> ([Matrix], [Matrix])
applyCorrectionsSilvaAlmeida SilvaAlmeidaParams{..} lastGradients lastStepWidths gradients weights =
	unzip $
	map `flip` (zip4 weights gradients lastGradients lastStepWidths) $
	\(w, gradient, lastGradient, lastStepWidth) ->
		let
			learnRates :: Matrix
			learnRates = Lina.fromLists $
				zipWith (zipWith conc)
				(Lina.toLists lastStepWidth) (Lina.toLists $ gradient * lastGradient)
				where
					conc lastStepWidthEntry signChange
						| signChange >= 0 = lastStepWidthEntry * silva_accelerateFactor
						| otherwise = lastStepWidthEntry * silva_decelerateFactor
		in
			(w - learnRates * gradient, learnRates)

applyCorrectionsDefault DefaultLearningParams{..} lastGradients gradients weights =
	map `flip` (zip3 weights gradients lastGradients) $ \(w, gradient, lastGradient) ->
		w - learnRate `Lina.scale` gradient + dampingFactor `Lina.scale` lastGradient

gradientsFromSample ::
	(Vector, Vector)
	-> ClassificationParam -> [Matrix]
gradientsFromSample (input, expectedOutput) weights =
		let
			outputs = reverse $ feedForward weights input :: [Vector] -- output for every stage of the network from (output to input)
			derivatives = -- output to input
				map (cmap sigmoidDerivFromRes) $
				(take (length outputs-1)) $ -- deletes input
				outputs
			(lastOutput:_) = outputs
			err = lastOutput - expectedOutput
		in
			backPropagate outputs derivatives err weights
		{-
		doLog $ "--------------------"
		doLog $ concat ["outputs: ", intercalate "\n" $ map show outputs]
		doLog $ concat ["derivs: ", intercalate "\n" $ map show derivatives]
		doLog $ concat ["err: ", show err]
		doLog $ concat ["corrections: ", intercalate "\n" $ map show corrections]
		-}

backPropagate ::
	[Vector] -- outputs (output to input)
	-> [Vector] -- derivatives (output to input)
	-> Vector -- error
	-> ClassificationParam -- network weights (output to input)
	-> [Matrix] -- gradients for every layer
backPropagate
		outputs
		derivatives
		err
		--weights
	=
	(flip (.)) reverse $ -- (before all: reverse weights)
	\weightsOutToIn -> -- weights from output to input...
		let
			-- deltas (from output to input):
			deltas :: [Vector]
			deltas =
				case derivatives of
					[] -> []
					(derivHead:derivRest) ->
						let deltaLast = derivHead * err
						in
							(deltaLast :) $
							snd $
							mapAccumL conc deltaLast (derivRest `zip` weightsOutToIn)
							where
								conc delta (deriv,weight) = twice $
									deriv * (removeLastRow weight #> delta)
			removeLastRow = (??(Lina.DropLast 1, Lina.All))
		--doLog $ "deltas dims: " ++ show (map Lina.size deltas)
		in
			reverse $
			flip map (zip deltas (drop 1 outputs)) $
			\(delta, output) ->
				Lina.tr (delta `Lina.outer` extendVec output)

-- |returns the temporary output for every stage of the nw: input = output^0, output^1, .., output^depth = output
feedForward ::
	ClassificationParam -> Vector
	-> [Vector]
feedForward weightMatrices input =
	(input:) $
	snd $
	mapAccumL conc input weightMatrices
	where
		conc inp weights = twice $ feedForward_oneStep weights inp

feedForward_oneStep :: Matrix -> Vector -> Vector
feedForward_oneStep weights input =
	Lina.cmap sigmoid $
	extendVec input <# weights


-----------------------------------------------------------------
-- helpers
-----------------------------------------------------------------

extendVec x = Lina.fromList $ Lina.toList x ++ [1]

networkMatrixDimensions :: Int -> [Int] -> [(Int,Int)]
networkMatrixDimensions inputDim_ dimensions =
	(map (+1) $ inputDim_ : dimensions) -- always one more row for the bias value
	`zip`
	dimensions
