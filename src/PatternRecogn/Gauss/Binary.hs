{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.Gauss.Binary where

import PatternRecogn.Lina
import PatternRecogn.Gauss.Utils
import PatternRecogn.Gauss.Types
import PatternRecogn.Types
import PatternRecogn.Utils

import Control.Monad.Random.Class
import Data.List( intercalate )
import Control.Monad.Identity


data ClassificationParam
	= ClassificationParam {
		min1 :: Vector,
		min2 :: Vector,
		covariance1 :: Matrix,
		covariance2 :: Matrix
	}

classesFromBinary ClassificationParam{..} =
	[Class min1 covariance1, Class min2 covariance2]

-----------------------------------------------------------------
-- fisher discriminant:
-----------------------------------------------------------------

findProjectionWithRnd :: MonadRandom m => ClassificationParam -> m Vector
findProjectionWithRnd param =
	flip findProjection param  <$> randomVec
	where
		randomVec = (vector . take (size $ min1 param)) <$> getRandoms

classifyProjected :: (Label, Label) -> Vector -> ClassificationParam -> Matrix -> VectorOf Label
classifyProjected labels projectionVec params =
	classify labels params
	.
	asColumn . (#> projectionVec) -- multiply every sample with the fisherVector

findProjection :: Vector -> ClassificationParam -> Vector
findProjection startVec params@ClassificationParam{..} =
	last $
	runIdentity $
	iterateWhileM (\i _ -> i < 1000) (const $ return . itFunc) $
	startVec
	where
		itFunc :: Vector -> Vector
		itFunc vec =
			unitary $ -- normalize
			scalar (
				((min1 - min2) <.> vec :: Double)
				/
				(fisherDiscr params vec :: Double)
			)
			*
			(inv (covariance1 + covariance2) #> (min1 - min2) :: Vector)

projectClasses projectionVec ClassificationParam{..} =
	ClassificationParam {
		min1 = scalar $ min1 <.> projectionVec,
		min2 = scalar $ min2 <.> projectionVec,
		covariance1 =
			scalar $
				projectionVec <.> (covariance1 #> projectionVec),
		covariance2 =
			scalar $
				projectionVec <.> (covariance2 #> projectionVec)
	}

fisherDiscr :: ClassificationParam -> Vector -> Double
fisherDiscr ClassificationParam{..} vec =
	(min1 <.> vec - min2 <.> vec) ^ 2
	/
	(vec <.> (covariance1 #> vec) + vec <.> (covariance2 #> vec))


-----------------------------------------------------------------
-- general gauss classification:
-----------------------------------------------------------------

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
