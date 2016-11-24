module PatternRecogn.Gauss.Utils where

import PatternRecogn.Types


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
