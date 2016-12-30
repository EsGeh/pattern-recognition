{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module TestBinary where

import Types
import Plot
import PatternRecogn.Types
import PatternRecogn.Lina as Lina

import qualified PatternRecogn.LinearRegression as LinearReg
--import PatternRecogn.Gauss.Binary
import qualified PatternRecogn.Gauss.Binary as Gauss
import qualified PatternRecogn.Perceptron as Perceptron

import Control.Monad.Random as Rand
import Data.List( intercalate )


data AlgorithmInput =
	AlgorithmInput {
		algInput_train1 :: Matrix,
		algInput_train2 :: Matrix,
		algInput_input :: Matrix
	}
	deriving( Show )

testPerceptron :: Monad m => Label -> Label -> (AlgorithmInput, Vector) -> m Double
testPerceptron =
	testWithAlg
		(\train1 train2 -> return $ Perceptron.calcClassificationParams train1 train2)
		(\labels param input -> return $ Perceptron.classify labels param input)

testGauss :: Monad m => Label -> Label -> (AlgorithmInput, Vector) -> m Double
testGauss =
	testWithAlg 
		(\train1 train2 -> return $ Gauss.calcClassificationParams train1 train2)
		classify
	where
		classify labels param input =
			return $ Gauss.classify labels param input

testLinearRegression :: Monad m => Label -> Label -> (AlgorithmInput, Vector) -> m Double
testLinearRegression =
	testWithAlg 
		(\train1 train2 -> return $ LinearReg.calcClassificationParams train1 train2)
		(\labels param input -> return $ LinearReg.classify labels param input)

testProjectedGauss :: Label -> Label -> (AlgorithmInput, Vector) -> ErrT IO Double
testProjectedGauss =
	testWithAlg 
		calcParam
		classify
	where
		calcParam train1 train2 =
			do
				let param = Gauss.calcClassificationParams train1 train2
				projectionVec <-
					Gauss.findProjectionWithRnd param
				let projectedClassificationParam =
					Gauss.projectClasses projectionVec param
				return $
					(projectedClassificationParam, projectionVec) 
		classify (label1, label2) (param, projectionVec) input =
			do
				let ret = Gauss.classifyProjected (label1,label2)
					projectionVec param
					input
				--liftIO $ putStrLn $ "plotting ..."
				plotProjected
					(concat $ ["plots/projectedGauss-", show label1, show label2, ".svg"])
					trainingProjected
					(Gauss.classesFromBinary param)
				return ret
			where
				trainingProjected =
					(#> projectionVec) input

testWithAlg ::
	Monad m =>
	(Matrix -> Matrix -> m param)
	-> ((Label, Label) -> param -> Matrix -> m (VectorOf Label))
	-> Label -> Label
	-> (AlgorithmInput, Vector)
	-> m Double
testWithAlg
		calcParam
		classify
		label1 label2
		(AlgorithmInput{..}, inputLabels)
	=
	do
		classificationParam <-
			calcParam algInput_train1 algInput_train2
		classified <-
			classify (label1,label2)
				classificationParam
				algInput_input
		return $
			calcClassificationQuality
				(cmap round $ inputLabels) classified

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

infoStringForParam_1D :: Gauss.ClassificationParam -> String
infoStringForParam_1D Gauss.ClassificationParam{..} =
	intercalate "\n" $
	[ concat $ ["min1:", show $ min1]
	, concat $ ["min2:", show $ min2 ]
	, concat $ ["cov1:", show $ covariance1 ]
	, concat $ ["cov2:", show $ covariance2 ]
	]
