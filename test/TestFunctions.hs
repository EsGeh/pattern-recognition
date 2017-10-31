{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module TestFunctions where

import AbstractTest
import Types

import qualified PatternRecogn.LinearRegression as LinearReg
import qualified PatternRecogn.Gauss.Classify as Gauss
import qualified PatternRecogn.Gauss.Projected as GaussProjected
import qualified PatternRecogn.Perceptron as Perceptron

--import Control.Monad.IO.Class
import Control.Monad.Random.Class


testPerceptron =
	testWithAlgBin
		(return . Perceptron.calcClassificationParams)
		(\param input -> return $ Perceptron.classify param input)

testGauss =
	testWithAlg 
		(\trainingData -> return $ Gauss.calcClassificationParams trainingData)
		(\param input ->
			return $ Gauss.classify param input)

testLinearRegression :: MonadIO m => TestDataBin -> m ()
testLinearRegression =
	testWithAlgBin 
		(\trainingData-> return $ LinearReg.calcClassificationParams trainingData)
		(\param input -> return $ LinearReg.classify param input)

testProjectedGauss ::
	forall m .
	(MonadIO m, MonadRandom m) =>
	TestDataBin -> m ()
testProjectedGauss =
	testWithAlgBin
		GaussProjected.calcClassificationParamsWithRnd
		(\param input -> return $ GaussProjected.classifyProjected param input)

	{-
	testWithAlgBin 
		calcParam
		classify
	where
		calcParam :: TrainingDataBin -> m Gauss.ClassificationParamBinary
		calcParam trainBinary =
			do
				let param@[(class1, label1),(class2,label2)] = Gauss.calcClassificationParams $ fromTrainingDataBin trainBinary
				projectionVec <-
					Gauss.findProjectionWithRnd (class1, class2)
				let projectedParam =
					Gauss.projectClasses projectionVec param
				return $
					(projectedClassificationParam, projectionVec) 
		classify ::
			Gauss.ClassificationParamBinary -> Matrix
			-> m (VectorOf Label)
		classify (param, projectionVec) input =
			do
				let ret = Gauss.classifyProjected (label1,label2)
					projectionVec param
					input
				--liftIO $ putStrLn $ "plotting ..."
				plotProjected
					(concat $ ["plots/projectedGauss-", show label1, show label2, ".svg"])
					trainingProjected
					(Gauss.fromBinary param)
				return ret
			where
				trainingProjected =
					(#> projectionVec) input
-}

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
