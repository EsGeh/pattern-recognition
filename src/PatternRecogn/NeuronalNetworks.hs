{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
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
	TrainingDataInternal,
	NetworkTrainingData(..),
	trainNetwork,
	initialNetwork, initialNetworkWithRnd,
	adjustWeightsBatch,
	{-
	adjustWeightsBatchDefault,
	adjustWeightsBatchSilvaAlmeida,
	-}
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

import Control.Monad.Random
import Control.Monad.Trans
import Data.Traversable( mapAccumL, forM )
import Data.List
import Data.Maybe



-----------------------------------------------------------------
-- high level api:
-----------------------------------------------------------------

calcClassificationParams ::
	(MonadLog m, MonadRandom m) =>
	LearningParams -> NetworkParams -> TrainingDataBundled -> m ClassificationParam
calcClassificationParams learningParams networkParams@NetworkParams{..} trainingData =
	fmap fst $
	trainNetwork
		networkParams
		[StopIfConverges 0.001]
		testWithTrainingData
		(adjustWeightsBatch learningParams trainingDataInternal)
		=<<
		initialNetworkWithRnd inputDim dims
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
		inputDim = Lina.size $ fst $ head $ trainingDataInternal

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
	forall m .
	(MonadLog m) =>
	NetworkParams
	-> [StopCond]
	-> (ClassificationParam -> R)
	-> (ClassificationParam -> TrainingMonadT m NetworkTrainingData)
	-> ClassificationParam
	-> m (ClassificationParam, StopReason)
trainNetwork NetworkParams{..} stopConds testWithTrainingData adjustWeights initNW =
	iterateWithCtxtM 1 updateNW
	$
	initialTrainingData initNW
	where

		updateNW :: 
			NetworkTrainingData
			-> IterationMonadT [NetworkTrainingData] m (Either (ClassificationParam, StopReason) NetworkTrainingData)
		updateNW networkTrainingData@NetworkTrainingData{ nwData_weights=network } =
			do
			continue <- cond network
			case continue of
				Nothing ->
					do
						ret <- liftToTrainingMonad adjustWeights $ networkTrainingData
						return $ Right ret
				Just stop -> return $ Left (network, stop)

		cond :: ClassificationParam -> IterationMonadT [NetworkTrainingData] m (Maybe StopReason)
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

initialNetworkWithRnd :: MonadRandom m => Int -> NetworkDimensions -> m ClassificationParam
initialNetworkWithRnd inputSize dimensions =
	forM (networkMatrixDimensions inputSize dimensions) $
	\(rowCount, colCount) ->
		(rowCount><colCount) <$> getRandomRs (0,1)

initialNetwork inputSize dimensions =
	map (Lina.konst 0) $ networkMatrixDimensions inputSize dimensions

{-
adjustWeightsBatchWithRnd ::
	forall m .
	(MonadLog m, MonadRandom m) =>
	LearningParams
	-> TrainingDataInternal ->
	ClassificationParam -> TrainingMonadT m NetworkTrainingData
adjustWeightsBatchWithRnd trainingParams trainingData weights =
	do
		randomVals <- lift $ (getRandomRs (0,1) :: m [R])
		adjustWeightsBatch_intern randomVals trainingParams trainingData weights
-}

{-
adjustWeightsBatch ::
	forall m .
	MonadLog m =>
	LearningParams
	-> TrainingDataInternal ->
	ClassificationParam -> TrainingMonadT m NetworkTrainingData
adjustWeightsBatch =
	adjustWeightsBatch_intern (repeat 0)
-}

adjustWeightsBatch either_learningParams =
	case either_learningParams of
		LearningParamsDefault learningParams ->
			adjustWeightsBatchDefault learningParams
		LearningParamsSilvaAlmeida learningParams ->
			adjustWeightsBatchSilvaAlmeida learningParams
		LearningParamsRProp learningParams ->
			adjustWeightsBatchRProp learningParams

adjustWeightsBatchSilvaAlmeida learningParams trainingData weights =
	askLastGradients >>= \(lastGradients:_) ->
	askLastStepWidths >>= \(lastStepWidths:_) ->
	lift $
	do
		(gradients :: [Matrix]) <- calcGradientsBatch weights trainingData
		let (newWeights, stepWidths) =
			applyCorrectionsSilvaAlmeida learningParams lastGradients lastStepWidths gradients weights
		return $
			NetworkTrainingData {
				nwData_weights = newWeights,
				nwData_gradients = gradients,
				nwData_stepWidths = stepWidths
			}

adjustWeightsBatchRProp learningParams trainingData weights =
	askLastGradients >>= \(lastGradients:_) ->
	askLastStepWidths >>= \(lastStepWidths:_) ->
	lift $
	do
		(gradients :: [Matrix]) <- calcGradientsBatch weights trainingData
		let (newWeights, stepWidths) =
			applyCorrectionsRProp learningParams lastGradients lastStepWidths gradients weights
		return $
			NetworkTrainingData {
				nwData_weights = newWeights,
				nwData_gradients = gradients,
				nwData_stepWidths = stepWidths
			}

adjustWeightsBatchDefault ::
	forall m .
	MonadLog m =>
	DefaultLearningParams
	-> TrainingDataInternal ->
	ClassificationParam -> TrainingMonadT m NetworkTrainingData
adjustWeightsBatchDefault learningParams trainingData weights =
	--askLastVals >>= \lastValues ->
	askLastGradients >>= \(lastGradients:_) ->
	lift $
	do
		(gradients :: [Matrix]) <- calcGradientsBatch weights trainingData
		let newWeights =
			applyCorrectionsDefault learningParams lastGradients gradients weights
		return $
			NetworkTrainingData {
				nwData_weights = newWeights,
				nwData_gradients = gradients,
				nwData_stepWidths = error "this error should never occur. Reason: stepWidth not used in default algorithm!"
			}

calcGradientsBatch ::
	forall m . MonadLog m =>
	ClassificationParam -> TrainingDataInternal -> m [Matrix]
calcGradientsBatch weights trainingData =
	combineGradients
	<$>
	(mapM (gradientsFromSample `flip` weights) trainingData :: m [[Matrix]])
	where
		combineGradients gradientsForEachSample =
			foldr1 (zipWith (+)) $
			gradientsForEachSample
			{-
			map (\(ws,rnd) -> (rnd `Lina.scale`) <$> ws) $
			gradientsForEachSample `zip` randomVec
			-}

{-
adjustWeightsOnLine ::
	forall m .
	MonadLog m =>
	LearningParams
	-> TrainingDataInternal ->
	NetworkTrainingData -> m NetworkTrainingData
adjustWeightsOnLine trainingParams =
	foldr (<=<) return .
	map f
	where
		f :: (Vector, Vector) -> NetworkTrainingData -> m NetworkTrainingData
		f sample NetworkTrainingData{ nwData_weights=weights, nwData_gradients=lastGradients } =
			do
				gradients <- gradientsFromSample sample weights
				return $
					NetworkTrainingData {
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
	MonadLog m =>
	(Vector, Vector)
	-> ClassificationParam -> m [Matrix]
gradientsFromSample (input, expectedOutput) weights =
	do
		let
			outputs = reverse $ feedForward weights input :: [Vector] -- output for every stage of the network from (output to input)
			derivatives = -- output to input
				map (cmap sigmoidDerivFromRes) $
				(take (length outputs-1)) $ -- deletes input
				outputs
			(lastOutput:_) = outputs
			err = lastOutput - expectedOutput
		gradients <- backPropagate outputs derivatives err weights
		{-
		doLog $ "--------------------"
		doLog $ concat ["outputs: ", intercalate "\n" $ map show outputs]
		doLog $ concat ["derivs: ", intercalate "\n" $ map show derivatives]
		doLog $ concat ["err: ", show err]
		doLog $ concat ["corrections: ", intercalate "\n" $ map show corrections]
		-}
		return gradients

backPropagate ::
	MonadLog m =>
	[Vector] -- outputs (output to input)
	-> [Vector] -- derivatives (output to input)
	-> Vector -- error
	-> ClassificationParam -- network weights (output to input)
	-> m [Matrix] -- gradients for every layer
backPropagate
		outputs
		derivatives
		err
		--weights
	=
	(flip (.)) reverse $ -- (before all: reverse weights)
	\weightsOutToIn -> -- weights from output to input...
	do
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
		return $
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

internalFromTrainingData OutputInterpretation{..} =
	map (mapToSnd $ labelToOutput)

internalFromBundledTrainingData OutputInterpretation{..} =
	map (mapToSnd $ labelToOutput)
	.
	fromBundled

extendVec x = Lina.fromList $ Lina.toList x ++ [1]

networkMatrixDimensions :: Int -> [Int] -> [(Int,Int)]
networkMatrixDimensions inputDim dimensions =
	(map (+1) $ inputDim : dimensions) -- always one more row for the bias value
	`zip`
	dimensions
