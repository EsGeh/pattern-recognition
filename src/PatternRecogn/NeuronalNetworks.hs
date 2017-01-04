{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.NeuronalNetworks(
	ClassificationParam,
	NetworkDimensions,
	OutputInterpretation(..), outputInterpretationMaximum, outputInterpretationSingleOutput,

	trainNetwork,
	calcClassificationParams,
	classify,

	-- low level api:
	initialNetwork, initialNetworkWithRnd,
	adjustWeightsBatch,
	adjustWeightsBatchWithRnd,
	adjustWeightsOnLine,
	paramsDiff,
	internalFromTrainingData,
	internalFromBundledTrainingData,
	TrainingDataInternal,
) where

import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils

import Control.Monad.Random
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Traversable( mapAccumL)
-- import Data.List( intercalate )


-- |represents the whole network
-- |list of weight-matrix for every layer
-- |	columns: weights for one perceptron
type ClassificationParam
	= [Matrix]

type NetworkDimensions = [Int]

type TrainingDataInternal =
	[(Vector,Vector)] -- sample, expected output

data OutputInterpretation =
	OutputInterpretation {
		outputToLabel :: Vector -> Label,
		labelToOutput :: Label -> Vector
	}

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

calcClassificationParams ::
	MonadLog m =>
	OutputInterpretation -> R -> NetworkDimensions -> TrainingDataBundled -> m ClassificationParam
calcClassificationParams outputInterpretation learnRate dims =
	trainNetwork dims learnRate
	.
	internalFromBundledTrainingData outputInterpretation

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

---

trainNetwork ::
	forall m .
	MonadLog m =>
	NetworkDimensions
	-> R
	-> TrainingDataInternal -> m ClassificationParam
trainNetwork dimensions learnRate sets =
	do
		doLog $ "initialNetwork dimensions: " ++ show (map Lina.size initNW)
		iterateWhileM_withCtxt 1 cond
			(lift . adjustWeightsBatch learnRate sets)
			initNW
	where
		cond weights = withIterationCtxt $ \it (prevWeights:_) ->
			return $
			it < 10000
			&&
			paramsDiff weights prevWeights >= 0.1
		initNW = initialNetwork inputDim dimensions
		inputDim =
			Lina.size $ fst $ head sets

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

twice a = (a,a)

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

-- helpers
---------------------------------------------

internalFromTrainingData OutputInterpretation{..} =
	map (mapToSnd $ labelToOutput)

internalFromBundledTrainingData OutputInterpretation{..} =
	map (mapToSnd $ labelToOutput)
	.
	fromBundled
{-
	join
	.
	map (uncurry toInternal)
	where
		toInternal :: Matrix -> Label -> TrainingDataInternal
		toInternal set label =
			Lina.toRows set `zip` repeat (labelToOutput label)
-}

extendVec x = Lina.fromList $ Lina.toList x ++ [1]

networkMatrixDimensions :: Int -> [Int] -> [(Int,Int)]
networkMatrixDimensions inputDim dimensions =
	(map (+1) $ inputDim : dimensions) -- always one more row for the bias value
	`zip`
	dimensions
