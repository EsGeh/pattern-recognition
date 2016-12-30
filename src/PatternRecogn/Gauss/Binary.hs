{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.Gauss.Binary where

import PatternRecogn.Lina
import PatternRecogn.Gauss.Utils
import PatternRecogn.Gauss.Types
import PatternRecogn.Types
import PatternRecogn.Utils

import Control.Monad.Random.Class
import Data.List( intercalate, maximumBy )
import Control.Monad.Identity


type ClassificationParam = (Class, Class)


fromBinary (c1, c2) = [c1, c2]


-----------------------------------------------------------------
-- fisher discriminant:
-----------------------------------------------------------------

findProjectionWithRnd :: MonadRandom m => ClassificationParam -> m Vector
findProjectionWithRnd param@(Class{ class_min = center },_) =
	flip findProjection param  <$> randomVec
	where
		randomVec = (vector . take (size $ center)) <$> getRandoms

classifyProjected :: (Label, Label) -> Vector -> ClassificationParam -> Matrix -> VectorOf Label
classifyProjected labels projectionVec params =
	classify labels params
	.
	asColumn . (#> projectionVec) -- multiply every sample with the fisherVector

findProjection :: Vector -> ClassificationParam -> Vector
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

projectClasses projectionVec (class1, class2) =
	map `flip` [class1, class2] $
	\Class{ class_min=center, class_cov=cov } ->
		Class{
			class_min = scalar $ center <.> projectionVec,
			class_cov =  scalar $ projectionVec <.> (cov #> projectionVec)
		}

fisherDiscr :: ClassificationParam -> Vector -> Double
fisherDiscr
		( Class{class_min=min1, class_cov=covariance1}
		, Class{class_min=min2, class_cov=covariance2}
		)
		vec
	=
		(min1 <.> vec - min2 <.> vec) ** 2
		/
		(vec <.> (covariance1 #> vec) + vec <.> (covariance2 #> vec))


-----------------------------------------------------------------
-- general gauss classification:
-----------------------------------------------------------------

calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	let
		[class1, class2] = map `flip` [set1, set2] $
			\set ->
				let
					center = average $ toRows set
				in
					Class{
						class_min = center,
						class_cov = cov_SAFE center set
					}
	in
		(class1, class2)

classify :: (Label, Label) -> ClassificationParam -> Matrix -> VectorOf Label
classify (labelNeg, labelPos) (class1, class2) =
	fromList
	.
	map classifySingleVec
	.
	toRows
	where
		classifySingleVec x =
			toLabels $
			retMaxIndex $
			flip map [class1, class2] $
			\Class{ class_min = center, class_cov = cov } ->
				mahalanobis center cov x
		retMaxIndex :: Ord a => [a] -> Int
		retMaxIndex l =
			snd $
			maximumBy (\x y -> fst x `compare` fst y) $
			l `zip` [0..]
		toLabels 0 = labelNeg
		toLabels 1 = labelPos
		toLabels _ = error "error classifying: index out of bounds!"

infoStringForParam :: ClassificationParam -> String
infoStringForParam =
	intercalate "\n"
	.
	map infoStringForSingleClass
	.
	fromBinary
	where
		infoStringForSingleClass Class{ class_min = center, class_cov = cov } =
			intercalate "\n" $
			[ concat $ ["center size:", show $ size center]
			, concat $ ["covariance size:", show $ size cov]
			]
