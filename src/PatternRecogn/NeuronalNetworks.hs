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

outputInterpretationMaximum count =
	OutputInterpretation{
		outputToLabel = fromIntegral . Lina.maxIndex,
		labelToOutput =
			\lbl -> Lina.fromList $ setElemAt (fromIntegral lbl) 1 $ replicate count 0
	}

networkMatrixDimensions :: Int -> [Int] -> [(Int,Int)]
networkMatrixDimensions inputDim dimensions =
	(map (+1) $ inputDim : dimensions) -- always one more row for the bias value
	`zip`
	dimensions

calcClassificationParams :: OutputInterpretation -> NetworkDimensions -> TrainingData -> ClassificationParam
calcClassificationParams OutputInterpretation{..} dims =
	trainNetwork dims
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
	(
		foldl (.) id $
		map feedForward_oneStep weightMatrices
	) input

trainNetwork :: NetworkDimensions -> TrainingDataInternal -> ClassificationParam
trainNetwork dimensions sets =
	last $
	runIdentity $
	iterateWhileM 1000 cond
		(return . adjustWeights sets)
		(map (Lina.konst 0) $ networkMatrixDimensions inputDims dimensions) -- initial network, all weights 0
	where
		cond (lastBeta:_) =
			True
			{-
			not $
			(
				(<=0.01) $ pnorm . cmap fromIntegral $
				(\x -> Lina.scalar 1 + x) $
				classify_extendedInput (-1,1) lastBeta $
					set1
			)
			&&
			(
				(<=0.01) $ pnorm . cmap fromIntegral $
				(\x -> Lina.scalar 1 - x) $
				classify_extendedInput (-1,1) lastBeta $
					set2
			)
			-}
		inputDims =
			Lina.size $ fst $ head sets

adjustWeights :: TrainingDataInternal -> ClassificationParam -> ClassificationParam
adjustWeights trainingData =
	foldl (.) id $ map adjustWeights_forOneSample trainingData

adjustWeights_forOneSample :: (Vector, Vector) -> ClassificationParam -> ClassificationParam
adjustWeights_forOneSample (input, expectedOutput) oldWeights =
	let
		outputs = feedForward oldWeights input :: [Vector] -- output for every stage of the network from (output to input)
		derivatives = map sigmoidDerivFromRes outputs
		lastOutput = fromMaybe input $ listToMaybe outputs
		err = lastOutput - expectedOutput
	in
		backPropagate outputs derivatives err oldWeights

-- (steps const)
backPropagate ::
	[Vector] -- outputs
	-> [Vector] -- derivatives
	-> Vector -- error
	-> ClassificationParam
	-> ClassificationParam
backPropagate
		outputs
		derivatives
		err
		weights
	=
		case (outputs,derivatives) of
			(_:outputRest, derivHead:derivRest) ->
				flip evalState (derivHead * err :: Vector) $
				forM (zip3 derivRest weights outputRest) $
					uncurry3 newWeight
			_ ->
				error $ concat $
				[ "length outputs: ", show $ length outputs
				, ", length derivatives:", show $ length derivatives
				]
	where
		newWeight
			:: Vector -> Matrix -> Vector -> State Vector Matrix
		newWeight d w o =
			do
				delta <- get
				put $ d * (w #> delta)
				return $ ((-1) * delta) `Lina.outer` o

feedForward ::
	ClassificationParam -> Vector
	-> [Vector]
feedForward weightMatrices =
{-
	snd $
	mapAccumL conc input weightMatrices
	where
		conc inp weights =
			let o = feedForward_oneStep weights inp
			in (o,o)
-}
	getDual . -- reverse list to make it start from output to input
	execWriter . (
		foldl (>=>) return $
		map ((\x -> (tell $ Dual [x]) >> return x) .) $
		map feedForward_oneStep weightMatrices
	)

feedForward_oneStep :: Matrix -> Vector -> Vector
feedForward_oneStep weights input =
	Lina.cmap sigmoid $
	(Lina.fromList $ Lina.toList input ++ [1]) <# weights

{-
feedForward_oneStep_withDeriv :: Matrix -> Vector -> (Vector, Vector)
feedForward_oneStep_withDeriv weights input =
	let output = feedForward_oneStep weights input
	in
		(output, Lina.cmap (\x -> x * (1 - x)) output)
-}
