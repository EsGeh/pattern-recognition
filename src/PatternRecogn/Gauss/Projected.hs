{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.Gauss.Projected where

import PatternRecogn.Gauss.Classify
import PatternRecogn.Gauss.Types

import PatternRecogn.Lina
import PatternRecogn.Types
import PatternRecogn.Utils

import Control.Monad.Random.Class
import Control.Monad.Identity


type ClassificationParamBinary = (Class, Class)

fromBinary (c1, c2) = [c1, c2]

-----------------------------------------------------------------
-- fisher discriminant:
-----------------------------------------------------------------

findProjectionWithRnd :: MonadRandom m => ClassificationParamBinary -> m Vector
findProjectionWithRnd param@(Class{ class_min = center },_) =
	flip findProjection param  <$> randomVec
	where
		randomVec = (vector . take (size $ center)) <$> getRandoms

classifyProjected :: (Label, Label) -> Vector -> ClassificationParamBinary -> Matrix -> VectorOf Label
classifyProjected (label1, label2) projectionVec (c1, c2) =
	classify [(c1, label1), (c2, label2)]
	.
	asColumn . (#> projectionVec) -- multiply every sample with the fisherVector

findProjection :: Vector -> ClassificationParamBinary -> Vector
findProjection
		startVec
		params@( Class{class_min=min1, class_cov=covariance1}
		, Class{class_min=min2, class_cov=covariance2}
		)
	=
	runIdentity $
	iterateWhileM_withCtxt 0 (\_ -> askIt >>= \i -> return $ i < 1000) (return . itFunc) $
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

projectClasses projectionVec =
	map $
	\Class{ class_min=center, class_cov=cov } ->
		Class{
			class_min = scalar $ center <.> projectionVec,
			class_cov =  scalar $ projectionVec <.> (cov #> projectionVec)
		}

fisherDiscr :: ClassificationParamBinary -> Vector -> Double
fisherDiscr
		( Class{class_min=min1, class_cov=covariance1}
		, Class{class_min=min2, class_cov=covariance2}
		)
		vec
	=
		(min1 <.> vec - min2 <.> vec) ** 2
		/
		(vec <.> (covariance1 #> vec) + vec <.> (covariance2 #> vec))
