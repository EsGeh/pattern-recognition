{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.LinearRegression where

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )
import qualified Numeric.LinearAlgebra as Lina

import Foreign.C.Types( CInt )


type Matrix = Lina.Matrix Double
type Vector = Lina.Vector Double
type Label = CInt


calcBeta :: Matrix -> Matrix -> Vector
calcBeta set1 set2 =
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

classify :: (Label, Label) -> Vector -> Matrix -> Lina.Vector Label
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

calcClassificationQuality :: Lina.Vector Label -> Lina.Vector Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

prependOnes m =
	konst 1 (rows m,1) ||| m
