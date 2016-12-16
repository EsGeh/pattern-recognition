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

outputInterpretationMaximum count =
	OutputInterpretation{
		outputToLabel =
			fromIntegral . Lina.maxIndex,
		labelToOutput =
			\lbl -> Lina.fromList $
				setElemAt (fromIntegral lbl) 1 $ replicate count 0
	}

networkMatrixDimensions :: Int -> [Int] -> [(Int,Int)]
networkMatrixDimensions inputDim dimensions =
	(map (+1) $ inputDim : dimensions) -- always one more row for the bias value
	`zip`
	dimensions

calcClassificationParams ::
	MonadLog m =>
	OutputInterpretation -> NetworkDimensions -> TrainingData -> m ClassificationParam
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
	last $ feedForward weightMatrices input

trainNetwork ::
	MonadLog m =>
	NetworkDimensions
	-> TrainingDataInternal -> m ClassificationParam
trainNetwork dimensions sets =
	do
		doLog $ "initialNetwork dimensions: " ++ show (map Lina.size initialNetwork)
		last <$>
			iterateWhileM 1000 cond
				(adjustWeights sets)
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
	TrainingDataInternal ->
	ClassificationParam -> m ClassificationParam
adjustWeights trainingData =
	foldl (>=>) return $ map adjustWeights_forOneSample $
		trainingData

adjustWeights_forOneSample ::
	MonadLog m =>
	(Vector, Vector)
	-> ClassificationParam -> m ClassificationParam
adjustWeights_forOneSample (input, expectedOutput) weights =
	do
		let
			outputs = reverse $ feedForward weights input :: [Vector] -- output for every stage of the network from (output to input)
			derivatives = map (cmap sigmoidDerivFromRes) $ (take (length outputs-1)) outputs
			(lastOutput:_) = outputs
			err = lastOutput - expectedOutput
		{-
		doLog $ "outputs size: " ++ show (map Lina.size outputs)
		doLog $ "derivatives size: " ++ show (map Lina.size derivatives)
		doLog $ "expectedOutput size: " ++ show (Lina.size expectedOutput)
		doLog $ "err size: " ++ show (Lina.size err)
		-}
		res <- backPropagate outputs derivatives err weights
		--doLog $ "new dimensions: " ++ show (map Lina.size res)
		return res

-- (steps const)
backPropagate ::
	MonadLog m =>
	[Vector] -- outputs
	-> [Vector] -- derivatives
	-> Vector -- error
	-> ClassificationParam
	-> m ClassificationParam
backPropagate
		outputs
		derivatives
		err
		--weights
	=
	(flip (.)) (
		reverse
		-- .
		-- map (??(Lina.DropLast 1, Lina.All))
	) $
	\weightsOutToIn ->
	do
		let
			removeLastRow = (??(Lina.DropLast 1, Lina.All))
			-- deltas (from output to input):
			deltas :: [Vector]
			deltas =
				case derivatives of
					[] -> []
					(derivHead:derivRest) ->
						let deltaLast = derivHead * err
						in
							(deltaLast :) $
							evalState `flip` deltaLast $
							forM (derivRest `zip` weightsOutToIn) $
								\(deriv, weight) -> get >>=
								\delta ->
									let newDelta =
										(deriv * removeLastRow weight #> delta)
									in
										put newDelta >> return newDelta
		--doLog $ "deltas dims: " ++ show (map Lina.size deltas)
		return $
			reverse $
			flip map (zip3 weightsOutToIn deltas (drop 1 outputs)) $
			\(weight, delta, output) ->
				weight - Lina.tr (delta `Lina.outer` extendVec output)
	{-
		let
			stageParams =
				zip3
					derivatives
					(reverse weights)
					outputs
		in
			evalState `flip` (derivHead * err :: Vector) $
				forM stageParams $
					uncurry3 newWeight
	where
		newWeight
			:: Vector -> Matrix -> Vector
			-> State Vector Matrix -- the state is the δ
		newWeight d w o =
			get >>= \delta ->
				do
					put $ d * (w #> delta)
					return $ ((-1) * delta) `Lina.outer` o
		-}

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
{-
	getDual . -- reverse list to make it start from output to input
	execWriter . (
		foldl (>=>) return $
		map ((\x -> (tell $ Dual [x]) >> return x) .) $
		map feedForward_oneStep weightMatrices
	)
-}

feedForward_oneStep :: Matrix -> Vector -> Vector
feedForward_oneStep weights input =
	Lina.cmap sigmoid $
	extendVec input <# weights

extendVec x = Lina.fromList $ Lina.toList x ++ [1]
