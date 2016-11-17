{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.Gauss where

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )
import qualified Numeric.LinearAlgebra as Lina

import Data.List( intercalate )
import Foreign.C.Types( CInt )


type Matrix = Lina.Matrix Double
type Vector = Lina.Vector Double
type Label = CInt

data ClassificationParam
	= ClassificationParam {
		min1 :: Vector,
		min2 :: Vector,
		covariance1 :: Matrix,
		covariance2 :: Matrix
	}

calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	ret
	where
		ret = ClassificationParam {
			min1 = average $ toRows set1,
			min2 = average $ toRows set2,
			covariance1 = cov_SAFE (min1 ret) set1,
			covariance2 = cov_SAFE (min2 ret) set2
		}

average x =
	(/ fromIntegral (length x)) $
	sum $
	x

cov_SAFE min set =
	if det cov > 0.01
	then cov
	else cov + alpha * ident (rows cov)
	where
		cov = covariance min set
		alpha = 0.01

covariance :: Vector -> Matrix -> Matrix
covariance min set =
	let
		centeredAroundMin = set - repmat (asRow min) countSamples 1
		countSamples = rows set
	in
		(/ fromIntegral countSamples) $
		sum $
		map (\v -> v `outer` v) $
		toRows $
		centeredAroundMin

mahalanobis :: Vector -> Matrix -> Vector -> R
mahalanobis min cov x =
	let
		centeredX = x - min
		inputDist =
			centeredX `dot` (inv cov #> centeredX)
	in
		1/sqrt (det (2 * pi * cov))
		*
		exp (-1/2 * inputDist)

classify :: (Label, Label) -> ClassificationParam -> Matrix -> Lina.Vector Label
classify (labelNeg, labelPos) ClassificationParam{..} =
	fromList
	.
	map classifySingleVec
	.
	toRows
	where
		classifySingleVec :: Vector -> Label
		classifySingleVec x =
			if
				mahalanobis min1 covariance1 x > mahalanobis min2 covariance2 x
			then
				labelNeg
			else
				labelPos

infoStringForParam :: ClassificationParam -> String
infoStringForParam ClassificationParam{..} =
	intercalate "\n" $
	[ concat $ ["min1 size:", show $ size min1]
	, concat $ ["min2 size:", show $ size min2 ]
	, concat $ ["cov1 size:", show $ size covariance1 ]
	, concat $ ["cov2 size:", show $ size covariance2 ]
	]

-- helper functions

calcClassificationQuality :: Lina.Vector Label -> Lina.Vector Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

prependOnes m =
	konst 1 (rows m,1) ||| m
