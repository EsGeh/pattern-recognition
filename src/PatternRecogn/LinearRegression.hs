{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.LinearRegression where

import PatternRecogn.Lina
import PatternRecogn.Types
import PatternRecogn.Utils


type ClassificationParam = (Vector, (Label,Label))

calcClassificationParams :: TrainingDataBin -> ClassificationParam
calcClassificationParams trainingDataBin =
	let
		[(set1,label1), (set2,label2)] = fromTrainingDataBin trainingDataBin
	in
		(\x -> (x,(label1,label2))) $
		flatten $ -- matrix to vector
		let
			x =
				extendInputData $
					set1
					===
					set2
			y =
				konst (-1) (count1,1)
				===
				konst 1 (count2,1)
			count1 = rows set1
			count2 = rows set2
			xTr_times_x_SAFE =
				let
					xTr_times_x = tr' x <> x
				in
					if det xTr_times_x > 0.01
					then xTr_times_x
					else
						xTr_times_x + (alpha * ident (rows xTr_times_x))
						where alpha = 0.01
		in
			inv xTr_times_x_SAFE <> tr' x <> y


classify :: ClassificationParam -> Matrix -> VectorOf Label
classify (beta,(labelNeg, labelPos)) input =
	cmap (assignLabels . sgn) $
		extendInputData input #> beta
	where
		sgn :: Double -> Double
		sgn x =
			let temp = signum x in
				if temp /= 0 then temp else 1
		assignLabels x
			| x < 0 = labelNeg
			| otherwise = labelPos

infoStringForParam :: ClassificationParam -> String
infoStringForParam (beta,_) =
	concat $ ["beta size:", show $ size beta]
