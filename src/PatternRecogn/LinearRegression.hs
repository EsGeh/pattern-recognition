{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.LinearRegression where


import PatternRecogn.Lina
import PatternRecogn.Types

type ClassificationParam = Vector

calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	flatten $ -- matrix to vector
	let
		x =
			prependOnes $
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

	
classify :: (Label, Label) -> ClassificationParam -> Matrix -> VectorOf Label
classify (labelNeg, labelPos) beta input =
	cmap (assignLabels . sgn) $
		prependOnes input #> beta
	where
		sgn :: Double -> Double
		sgn x =
			let temp = signum x in
				if temp /= 0 then temp else 1
		assignLabels x
			| x < 0 = labelNeg
			| otherwise = labelPos

infoStringForParam :: ClassificationParam -> String
infoStringForParam beta =
	concat $ ["beta size:", show $ size beta]

-- | helper functions

prependOnes m =
	konst 1 (rows m,1) ||| m
