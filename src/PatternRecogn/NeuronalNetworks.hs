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
import Data.Traversable( for )


-- |list of weight-matrix for every layer
-- |	columns: weights for one perceptron
type ClassificationParam
	= [Matrix]

type NetworkDimensions = [Int]

type TrainingData = [(Matrix,Label)]
type TrainingDataInternal = [(Vector,Vector)]

data OutputInterpretation =
	OutputInterpretation {
		outputToLabel :: Vector -> Label,
		labelToOutput :: Label -> Vector
	}

calcClassificationParams :: OutputInterpretation -> NetworkDimensions -> TrainingData -> ClassificationParam
calcClassificationParams OutputInterpretation{..} dims =
	trainNetwork dims
	.
	join
	.
	map (uncurry temp)
	.
	map (\(matr, lbl) -> (prependOnes matr, lbl)) -- extend training data
	where
		temp :: Matrix -> Label -> TrainingDataInternal
		temp set label =
			Lina.toRows set `zip` repeat (labelToOutput label)

trainNetwork :: NetworkDimensions -> TrainingDataInternal -> ClassificationParam
trainNetwork dimensions =
	calcClassificationParams_extendedVecs dimensions

{-
	calcClassificationParams_extendedVecs
		(prependOnes set1)
		(prependOnes set2)
-}

calcClassificationParams_extendedVecs :: NetworkDimensions -> TrainingDataInternal -> ClassificationParam
calcClassificationParams_extendedVecs dimensions sets =
	last $
	runIdentity $
	iterateWhileM 1000 cond
		(return . adjustWeights sets)
		(map (Lina.konst 0) initDims)
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
		initDims :: [(Int,Int)]
		initDims =
			(inputDims : dimensions)
			`zip`
			dimensions
		inputDims = Lina.size $ fst $ head sets

pnorm :: Vector -> R
pnorm x =
	sqrt $ x <.> x

adjustWeights :: TrainingDataInternal -> ClassificationParam -> ClassificationParam
adjustWeights trainingData =
	foldl (.) id $
	map adjustWeights_forOneSample trainingData

adjustWeights_forOneSample :: (Vector, Vector) -> ClassificationParam -> ClassificationParam
adjustWeights_forOneSample (input, expectedOutput) oldWeights =
	let
		outputs = feedForward oldWeights input
		derivatives = map sigmoidDerivFromRes outputs
		lastOutput = fromMaybe input $ listToMaybe outputs
		err = lastOutput - expectedOutput
	in
		backPropagate outputs derivatives err oldWeights

uncurry3 f (a,b,c) = f a b c

-- (steps const)
backPropagate ::
	[Vector] -- outputs
	-> [Vector] -- derivatives
	-> Vector -- error
	-> ClassificationParam
	-> ClassificationParam
backPropagate outputs@(_:outputRest) derivatives@(derivHead:derivRest) err weights =
	flip evalState (derivHead * err :: Vector) $
	forM (zip3 derivRest weights outputRest) $
		uncurry3 newWeight
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
	getDual . -- reverse list to make it start from output to input
	execWriter . (
		foldl (>=>) return $
		map ((\x -> (tell $ Dual [x]) >> return x) .) $
		map feedForward_oneStep weightMatrices
	)

classify :: ClassificationParam -> Matrix -> VectorOf Label
classify param =
	classify_extendedInput param . prependOnes

classify_extendedInput :: ClassificationParam -> Matrix -> VectorOf Label
classify_extendedInput param input =
	Lina.fromList $
	map (toEnum . Lina.maxIndex) $
	map (classifySingleSample_extended param) $
	Lina.toRows $
	input

classifySingleSample_extended :: ClassificationParam -> Vector-> Vector
classifySingleSample_extended weightMatrices input =
	(
		foldl (.) id $
		map feedForward_oneStep weightMatrices
	) input

feedForward_oneStep :: Matrix -> Vector -> Vector
feedForward_oneStep weights input =
	Lina.cmap sigmoid $
	input <# weights

feedForward_oneStep_withDeriv :: Matrix -> Vector -> (Vector, Vector)
feedForward_oneStep_withDeriv weights input =
	let output = feedForward_oneStep weights input
	in
		(output, Lina.cmap (\x -> x * (1 - x)) output)

sigmoid x = 1 / (1 + exp (-x))

sigmoidDerivFromRes x = x * (1 - x)

{-
sigmoidDeriv x =
	let s = sigmoid x
	in s * (1 - s)
-}

prependOnes m =
	konst 1 (rows m,1) ||| m
