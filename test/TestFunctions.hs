{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module TestFunctions where

import AbstractTest
import Types

import qualified PatternRecogn.LinearRegression as LinearReg
import qualified PatternRecogn.Perceptron as Perceptron

--import Control.Monad.IO.Class
import Control.Monad.Random.Class


testPerceptron =
	testWithAlgBin
		(return . Perceptron.calcClassificationParams)
		(\param input -> return $ Perceptron.classify param input)

testLinearRegression :: MonadIO m => TestDataBin -> m ()
testLinearRegression =
	testWithAlgBin 
		(\trainingData-> return $ LinearReg.calcClassificationParams trainingData)
		(\param input -> return $ LinearReg.classify param input)

{-
descriptionString
	set1 set2
	param 
	projectionVec projectedClassificationParam
	inputData classified result
	=
	unlines $
	[ concat $ ["set1 size:", show $ size set1]
	, concat $ ["set2 size:", show $ size set2]
	, Gauss.infoStringForParam param
	, concat $ ["projectionVec size:", show $ size projectionVec]
	, concat $ ["projected clusters: ----------------------------"]
	, infoStringForParam_1D projectedClassificationParam
	, concat $ ["inputData size:", show $ size inputData]
	, concat $ ["result: --------------------"]
	, concat $ ["classification quality:", show $ result]
	]
-}

{-
infoStringForParam_1D :: Gauss.ClassificationParam -> String
infoStringForParam_1D param =
	intercalate "\n" $
	[ concat $ ["min1:", show $ min1]
	, concat $ ["min2:", show $ min2 ]
	, concat $ ["cov1:", show $ covariance1 ]
	, concat $ ["cov2:", show $ covariance2 ]
	]
-}
