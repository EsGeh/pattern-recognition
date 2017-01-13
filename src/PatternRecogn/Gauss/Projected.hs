{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.Gauss.Projected(
	ClassificationParamBinary, ClassificationParamBinaryWithProjectionVec,

	calcParamBinary,
	calcClassificationParams, calcClassificationParamsWithRnd,
	classifyProjected,

	-- low level api:
	findProjection, findProjectionWithRnd,
	projectClasses, fisherDiscr,
) where

import qualified PatternRecogn.Gauss.Classify as Gauss
import PatternRecogn.Gauss.Types

import PatternRecogn.Lina
import PatternRecogn.Types
import PatternRecogn.Utils

import Control.Monad.Random.Class
import Control.Monad.Identity


type ClassificationParamBinary = (TwoClasses,(Label, Label))
type TwoClasses = (Class, Class)
type ClassificationParamBinaryWithProjectionVec = (ClassificationParamBinary, Vector)

--fromBinary (c1, c2) = [c1, c2]
toBinaryParam l =
	let binL = take 2 $ l
	in
		if length binL == 2
		then Just $
			let [(c1,label1), (c2,label2)] = binL
			in ((c1,c2),(label1,label2))
		else
			Nothing

calcParamBinary calcParam trainingData =
	let Just param =
		toBinaryParam $ calcParam $ fromTrainingDataBin $ trainingData
	in param

calcClassificationParamsWithRnd trainingData =
	let
		param@(classes,_) = calcParamBinary Gauss.calcClassificationParams trainingData
	in
		do
			projectionVec <- findProjectionWithRnd classes
			return $ (param, projectionVec)

calcClassificationParams initVec trainingData =
	let
		param@(classes,_) = calcParamBinary Gauss.calcClassificationParams trainingData
		projectionVec = findProjection initVec classes
	in
		(param, projectionVec)

-----------------------------------------------------------------
-- fisher discriminant:
-----------------------------------------------------------------

findProjectionWithRnd :: MonadRandom m => TwoClasses -> m Vector
findProjectionWithRnd param@(Class{ class_min = center },_) =
	flip findProjection param  <$> randomVec
	where
		randomVec = (vector . take (size $ center)) <$> getRandoms

classifyProjected :: ClassificationParamBinaryWithProjectionVec -> Matrix -> VectorOf Label
classifyProjected  (((c1, c2),(label1, label2)),projectionVec)  =
	Gauss.classify [(c1, label1), (c2, label2)]
	.
	asColumn . (#> projectionVec) -- multiply every sample with the fisherVector

findProjection :: Vector -> TwoClasses -> Vector
findProjection
		startVec
		params@(
			Class{class_min=min1, class_cov=covariance1}
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

fisherDiscr :: TwoClasses -> Vector -> Double
fisherDiscr
		( Class{class_min=min1, class_cov=covariance1}
		, Class{class_min=min2, class_cov=covariance2}
		)
		vec
	=
		(min1 <.> vec - min2 <.> vec) ** 2
		/
		(vec <.> (covariance1 #> vec) + vec <.> (covariance2 #> vec))
