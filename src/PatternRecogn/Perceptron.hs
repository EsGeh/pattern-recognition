module PatternRecogn.Perceptron where

import PatternRecogn.Lina as Lina
import PatternRecogn.Types
import PatternRecogn.Utils

import Control.Monad.Identity


type ClassificationParam
	= Vector


calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	last $
	runIdentity $
	iterateWhileM 1000 cond
		(return . perceptronStepAll set1 set2)
		(Lina.konst 1 $ Lina.cols set1)
	where
		cond (lastBeta:_) =
			(
				(<=0.05) $ pnorm . cmap fromIntegral $
				(\x -> Lina.scalar 1 + x) $
				classify (-1,1) lastBeta $
					set1
			)
			&&
			(
				(<=0.05) $ pnorm . cmap fromIntegral $
				(\x -> Lina.scalar 1 - x) $
				classify (-1,1) lastBeta $
					set2
			)

pnorm :: Vector -> R
pnorm x =
	sqrt $ x <.> x

-- TODO: interleave both sets and randomize permutation:
perceptronStepAll set1 set2 param =
	perceptronStep (-1) set1
	.	
	perceptronStep 1 set2
	$
	param

perceptronStep :: Label -> Matrix -> ClassificationParam -> ClassificationParam
perceptronStep expectedLabel set param =
	foldl conc param $ Lina.toRows set
	where
		conc :: ClassificationParam -> Vector -> ClassificationParam
		conc beta y = 
			let estimatedClass = classifySingleSample (-1, 1) beta y
			in
				if estimatedClass == expectedLabel
				then beta
				else
					if estimatedClass == 0
					then beta + y
					else beta - y

classify :: (Label, Label) -> ClassificationParam -> Matrix -> VectorOf Label
classify labels beta input =
	Lina.fromList $
	map (classifySingleSample labels beta) $
	Lina.toRows input

classifySingleSample :: (Label, Label) -> ClassificationParam -> Vector-> Label
classifySingleSample (labelNeg, labelPos) beta input =
	-- not correct yet...
	-- TODO:
	if (beta <.> input) >= 0
	then labelPos
	else labelNeg
