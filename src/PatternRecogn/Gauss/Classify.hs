{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.Gauss.Classify(
	ClassificationParam,

	calcClassificationParams,
	classify,
	infoStringForParam
) where

import PatternRecogn.Lina
import PatternRecogn.Gauss.Utils
import PatternRecogn.Gauss.Types
import PatternRecogn.Types
import PatternRecogn.Utils

import Data.List( intercalate, maximumBy )


type ClassificationParam = [(Class, Label)]

-----------------------------------------------------------------
-- general gauss classification:
-----------------------------------------------------------------

calcClassificationParams :: TrainingDataBundled -> ClassificationParam
calcClassificationParams trainingData =
	map `flip` trainingData $ mapToFst $
		\set ->
			let
				center = average $ toRows set
			in
				Class{
					class_min = center,
					class_cov = cov_SAFE center set
				}

classify :: ClassificationParam -> Matrix -> VectorOf Label
classify param =
	fromList
	.
	map classifySingleVec
	.
	toRows
	where
		classifySingleVec :: Vector -> Label
		classifySingleVec vec =
			snd $
			maximumBy (\x y -> fst x `compare` fst y) $
			map `flip` param $ mapToFst $
			\Class{ class_min = center, class_cov = cov } ->
				mahalanobis center cov vec

infoStringForParam :: ClassificationParam -> String
infoStringForParam =
	intercalate "\n"
	.
	map infoStringForSingleClass
	where
		infoStringForSingleClass (Class{ class_min = center, class_cov = cov },label) =
			(concat ["Label ", show label, ":"] ++) $
			intercalate "\n" $
			[ concat $ ["center size:", show $ size center]
			, concat $ ["covariance size:", show $ size cov]
			]
