{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.Perceptron where

import PatternRecogn.Lina as Lina
import PatternRecogn.Types
import PatternRecogn.Utils

import Control.Monad.Identity


type ClassificationParam
	= Vector


calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	calcClassificationParams_extendedVecs
		(extendInputData set1)
		(extendInputData set2)

calcClassificationParams_extendedVecs :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams_extendedVecs set1 set2 =
	last $
	runIdentity $
	iterateWhileM 1000 cond
		(return . perceptronStepAll set1 set2)
		(Lina.konst 0 $ Lina.cols set1)
	where
		cond (lastBeta:_) =
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
			let estimatedClass = classifySingleSample_extended (-1, 1) beta y
			in
				if estimatedClass == expectedLabel
				then beta
				else
					if estimatedClass < 0
					then beta + y
					else beta - y

classify :: (Label, Label) -> ClassificationParam -> Matrix -> VectorOf Label
classify labels beta =
	classify_extendedInput labels beta . extendInputData

classify_extendedInput :: (Label, Label) -> ClassificationParam -> Matrix -> VectorOf Label
classify_extendedInput labels beta input =
	Lina.fromList $
	map (classifySingleSample_extended labels beta) $
	Lina.toRows $
	input

classifySingleSample_extended :: (Label, Label) -> ClassificationParam -> Vector-> Label
classifySingleSample_extended (labelNeg, labelPos) beta input =
	if (beta <.> input) >= 0
	then labelPos
	else labelNeg
