{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.NeuronalNetworks where

import PatternRecogn.Lina as Lina
import PatternRecogn.Types
import PatternRecogn.Utils

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer hiding( (<>) )
import Data.Maybe
import Data.Traversable( for, mapAccumL)
import Data.List( intercalate )


-- |represents the whole network
-- |list of weight-matrix for every layer
-- |	columns: weights for one perceptron
type ClassificationParam
	= [Matrix]

type NetworkDimensions = [Int]

type TrainingData =
	[(Matrix,Label)]

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

calcClassificationParams ::
	MonadLog m =>
	OutputInterpretation -> R -> NetworkDimensions -> TrainingData -> m ClassificationParam
calcClassificationParams OutputInterpretation{..} learnRate dims =
	trainNetwork dims learnRate
	.
	join
	.
	map (uncurry toInternal)
	where
		toInternal :: Matrix -> Label -> TrainingDataInternal
		toInternal set label =
			Lina.toRows set `zip` repeat (labelToOutput label)

classify :: OutputInterpretation -> ClassificationParam -> Matrix -> VectorOf Label
classify OutputInterpretation{..} param input =
	Lina.fromList $
	map outputToLabel $
	map (classifySingleSample param) $
	Lina.toRows $
	input

classifySingleSample :: ClassificationParam -> Vector-> Vector
classifySingleSample weightMatrices input =
	last $ feedForward weightMatrices input

trainNetwork ::
	MonadLog m =>
	NetworkDimensions
	-> R
	-> TrainingDataInternal -> m ClassificationParam
trainNetwork dimensions learnRate sets =
	do
		doLog $ "initialNetwork dimensions: " ++ show (map Lina.size initialNetwork)
		head <$>
			iterateWhileM_ 2 10000 cond
				(adjustWeights learnRate sets)
				initialNetwork
	where
		cond (lastBeta:_) =
			True -- <- TODO!!!
		initialNetwork =
			map (Lina.konst 0) $ networkMatrixDimensions inputDims dimensions
		inputDims =
			Lina.size $ fst $ head sets

adjustWeights ::
	MonadLog m =>
	R
	-> TrainingDataInternal ->
	ClassificationParam -> m ClassificationParam
adjustWeights learnRate =
	foldl (>=>) return .
	map (adjustWeights_forOneSample learnRate)

adjustWeights_forOneSample ::
	MonadLog m =>
	R
	-> (Vector, Vector)
	-> ClassificationParam -> m ClassificationParam
adjustWeights_forOneSample learnRate (input, expectedOutput) weights =
	do
		let
			outputs = reverse $ feedForward weights input :: [Vector] -- output for every stage of the network from (output to input)
			derivatives =
				map (cmap sigmoidDerivFromRes) $
				(take (length outputs-1)) outputs
			(lastOutput:_) = outputs
			err = lastOutput - expectedOutput
		{-
		doLog $ "outputs size: " ++ show (map Lina.size outputs)
		doLog $ "derivatives size: " ++ show (map Lina.size derivatives)
		doLog $ "expectedOutput size: " ++ show (Lina.size expectedOutput)
		doLog $ "err size: " ++ show (Lina.size err)
		-}
		res <- backPropagate learnRate outputs derivatives err weights
		--doLog $ "new dimensions: " ++ show (map Lina.size res)
		return res

-- (steps const)
backPropagate ::
	MonadLog m =>
	R -- learnRate
	-> [Vector] -- outputs
	-> [Vector] -- derivatives
	-> Vector -- error
	-> ClassificationParam
	-> m ClassificationParam
backPropagate
		learnRate
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
									deriv * removeLastRow weight #> delta
								twice a = (a,a)
			removeLastRow = (??(Lina.DropLast 1, Lina.All))
		--doLog $ "deltas dims: " ++ show (map Lina.size deltas)
		return $
			reverse $
			flip map (zip3 weightsOutToIn deltas (drop 1 outputs)) $
			\(weight, delta, output) ->
				weight - Lina.tr ((learnRate `Lina.scale` delta) `Lina.outer` extendVec output)

feedForward ::
	ClassificationParam -> Vector
	-> [Vector]
feedForward weightMatrices input =
	(input:) $
	snd $
	mapAccumL conc input weightMatrices
	where
		conc inp weights =
			let o = feedForward_oneStep weights inp
			in (o,o)

feedForward_oneStep :: Matrix -> Vector -> Vector
feedForward_oneStep weights input =
	Lina.cmap sigmoid $
	extendVec input <# weights

-- helpers
---------------------------------------------

extendVec x = Lina.fromList $ Lina.toList x ++ [1]

networkMatrixDimensions :: Int -> [Int] -> [(Int,Int)]
networkMatrixDimensions inputDim dimensions =
	(map (+1) $ inputDim : dimensions) -- always one more row for the bias value
	`zip`
	dimensions
