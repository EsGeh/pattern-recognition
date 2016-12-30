module PatternRecogn.Gauss.Utils where

import PatternRecogn.Types


average x =
	(/ fromIntegral (length x)) $
	sum $
	x

cov_SAFE center set =
	if det cov > 0.01
	then cov
	else cov + alpha * ident (rows cov)
	where
		cov = covariance center set
		alpha = 0.01

covariance :: Vector -> Matrix -> Matrix
covariance center set =
	let
		centered = set - repmat (asRow center) countSamples 1
		countSamples = rows set
	in
		(/ fromIntegral countSamples) $
		sum $
		map (\v -> v `outer` v) $
		toRows $
		centered

mahalanobis :: Vector -> Matrix -> Vector -> R
mahalanobis center cov x =
	let
		centeredX = x - center
		inputDist =
			centeredX `dot` (inv cov #> centeredX)
	in
		1/sqrt (det (2 * pi * cov))
		*
		exp (-1/2 * inputDist)
