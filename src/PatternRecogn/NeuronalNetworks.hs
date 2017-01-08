{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.NeuronalNetworks(
	ClassificationParam,
	NetworkParams(..),
	NetworkDimensions,
	StopCond(..), StopReason(..),
	OutputInterpretation(..), outputInterpretationMaximum, outputInterpretationSingleOutput,

	calcClassificationParams,
	classify,

	-- low level api:
	TrainingDataInternal,
	trainNetwork,
	initialNetwork, initialNetworkWithRnd,
	adjustWeightsBatch,
	adjustWeightsBatchWithRnd,
	adjustWeightsOnLine,
	paramsDiff,
	internalFromTrainingData,
	internalFromBundledTrainingData,
) where

import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils

import Control.Monad.Random
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Traversable( mapAccumL)
import Data.Maybe


-----------------------------------------------------------------
-- Types:
-----------------------------------------------------------------

-- |represents the whole network
-- |list of weight-matrix for every layer
-- |	columns: weights for one perceptron
type ClassificationParam
	= [Matrix]

data NetworkParams
	= NetworkParams {
		dims :: NetworkDimensions,
		outputInterpretation :: OutputInterpretation
	}

type NetworkDimensions = [Int]

type TrainingDataInternal =
	[(Vector,Vector)] -- sample, expected output

data OutputInterpretation =
	OutputInterpretation {
		outputToLabel :: Vector -> Label,
		labelToOutput :: Label -> Vector
	}

data StopCond
	= StopAfterMaxIt Int
	| StopIfConverges R
	| StopIfQualityReached R

data StopReason
	= StopReason_MaxIt Int
	| StopReason_Converged R Int
	| StopReason_QualityReached R Int

-- | 0 <-> (1,0,0,...); 3 <-> (0,0,0,1,0,...)
outputInterpretationMaximum count =
	OutputInterpretation{
		outputToLabel =
			fromIntegral . Lina.maxIndex,
		labelToOutput =
			\lbl -> Lina.fromList $
				setElemAt (fromIntegral lbl) 1 $ replicate count 0
	}

outputInterpretationSingleOutput =
	OutputInterpretation{
		outputToLabel = \x ->
			case Lina.toList x of
				[output] ->
					round output
					--if output >= 0.5 then 1 else 0
				_ -> error "outputInterpretation: error!",
		labelToOutput =
			\lbl -> Lina.fromList [fromIntegral lbl]
	}

-- | sums up element wise differences
paramsDiff newWeights weights =
	sum $
	map (sum . join . toLists . cmap abs) $
	zipWith (-) newWeights weights


-----------------------------------------------------------------
-- high level api:
-----------------------------------------------------------------

calcClassificationParams ::
	(MonadLog m, MonadRandom m) =>
	R -> NetworkParams -> TrainingDataBundled -> m ClassificationParam
calcClassificationParams learnRate networkParams@NetworkParams{..} trainingData =
	fmap fst $
	trainNetwork
		networkParams
		[StopIfConverges 0.001]
		testWithTrainingData
		(lift . adjustWeightsBatchWithRnd learnRate trainingDataInternal)
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
	-> (ClassificationParam -> IterationMonadT [ClassificationParam] m ClassificationParam)
	-> ClassificationParam
	-> m (ClassificationParam, StopReason)
trainNetwork NetworkParams{..} stopConds testWithTrainingData adjustWeights initNW =
	iterateWithCtxtM 1 updateNW
	$
	initNW
	where

		updateNW :: 
			ClassificationParam
			-> IterationMonadT [ClassificationParam] m (Either (ClassificationParam, StopReason) ClassificationParam)
		updateNW network =
			do
			continue <- cond network
			case continue of
				Nothing ->
					do
						ret <- updateNW' network
						return $ Right ret
				Just stop -> return $ Left (network, stop)
			where
				updateNW' :: 
					ClassificationParam
					-> IterationMonadT [ClassificationParam] m ClassificationParam
				updateNW' nw =
					adjustWeights nw

		cond :: ClassificationParam -> IterationMonadT [ClassificationParam] m (Maybe StopReason)
		cond x = withIterationCtxt $ \it previousVals ->
			cond' it previousVals x
			where
				cond' it (previousVal:_) lastVal = return $
					(
						listToMaybe .
						catMaybes
					) $
					temp
					where
						temp :: [Maybe StopReason]
						temp =
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

adjustWeightsBatchWithRnd ::
	forall m .
	(MonadLog m, MonadRandom m) =>
	R
	-> TrainingDataInternal ->
	ClassificationParam -> m ClassificationParam
adjustWeightsBatchWithRnd learnRate trainingData weights =
	do
		randomVals <- getRandomRs (0,1) :: m [R]
		adjustWeightsBatch_intern randomVals learnRate trainingData weights

adjustWeightsBatch ::
	forall m .
	MonadLog m =>
	R
	-> TrainingDataInternal ->
	ClassificationParam -> m ClassificationParam
adjustWeightsBatch =
	adjustWeightsBatch_intern (repeat 0)

adjustWeightsBatch_intern ::
	forall m .
	MonadLog m =>
	[R]
	-> R
	-> TrainingDataInternal ->
	ClassificationParam -> m ClassificationParam
adjustWeightsBatch_intern randomVec learnRate trainingData weights =
	do
		(corrections :: [Matrix]) <-
			combineCorrections
			<$>
			(mapM (correctionsFromSample `flip` weights) trainingData :: m [[Matrix]])
		return $ applyCorrections learnRate corrections weights
	where
		combineCorrections correctionsForEachSample =
			foldr1 (zipWith (+)) $
			map (\(ws,rnd) -> (rnd `Lina.scale`) <$> ws) $
			correctionsForEachSample `zip` randomVec

adjustWeightsOnLine ::
	forall m .
	MonadLog m =>
	R
	-> TrainingDataInternal ->
	ClassificationParam -> m ClassificationParam
adjustWeightsOnLine learnRate =
	foldr (<=<) return .
	map f
	where
		f :: (Vector, Vector) -> ClassificationParam -> m ClassificationParam
		f sample weights =
			do
				weightUpdates <- correctionsFromSample sample weights
				return $ applyCorrections learnRate weightUpdates weights

applyCorrections learnRate weightsDeltas weights =
	map `flip` (weights `zip` weightsDeltas) $ \(w, delta) -> w - learnRate `Lina.scale` delta

correctionsFromSample ::
	MonadLog m =>
	(Vector, Vector)
	-> ClassificationParam -> m [Matrix]
correctionsFromSample (input, expectedOutput) weights =
	do
		let
			outputs = reverse $ feedForward weights input :: [Vector] -- output for every stage of the network from (output to input)
			derivatives = -- output to input
				map (cmap sigmoidDerivFromRes) $
				(take (length outputs-1)) $ -- deletes input
				outputs
			(lastOutput:_) = outputs
			err = lastOutput - expectedOutput
		corrections <- backPropagate outputs derivatives err weights
		{-
		doLog $ "--------------------"
		doLog $ concat ["outputs: ", intercalate "\n" $ map show outputs]
		doLog $ concat ["derivs: ", intercalate "\n" $ map show derivatives]
		doLog $ concat ["err: ", show err]
		doLog $ concat ["corrections: ", intercalate "\n" $ map show corrections]
		-}
		return corrections

backPropagate ::
	MonadLog m =>
	[Vector] -- outputs (output to input)
	-> [Vector] -- derivatives (output to input)
	-> Vector -- error
	-> ClassificationParam -- network weights (output to input)
	-> m [Matrix] -- derivatives
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
