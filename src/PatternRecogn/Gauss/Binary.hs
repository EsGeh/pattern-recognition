{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.Gauss.Binary where

import PatternRecogn.Lina
import PatternRecogn.Gauss.Utils
import PatternRecogn.Gauss.Types
import PatternRecogn.Types

import Data.List( intercalate )


data ClassificationParam
	= ClassificationParam {
		min1 :: Vector,
		min2 :: Vector,
		covariance1 :: Matrix,
		covariance2 :: Matrix
	}

-- fisher discriminant:

calcFisherDiscr :: ClassificationParam -> (Vector, ClassificationParam)
calcFisherDiscr = undefined
	-- "TODO"

classifyWithFisherDiscr :: (Label, Label) -> Vector -> ClassificationParam -> Matrix -> VectorOf Label
classifyWithFisherDiscr labels fisherVector params =
	classify labels params
	.
	asColumn . (#> fisherVector) -- multiply every sample with the fisherVector

-- general gauss classification:

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

classify :: (Label, Label) -> ClassificationParam -> Matrix -> VectorOf Label
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

calcClassificationQuality :: VectorOf Label -> VectorOf Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

prependOnes m =
	konst 1 (rows m,1) ||| m
