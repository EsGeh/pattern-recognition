{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module TestMultiple where

import Types
import Plot
import PatternRecogn.Types
import PatternRecogn.Lina as Lina

import qualified PatternRecogn.NeuronalNetworks as NN

import Control.Monad.Random as Rand
import Data.List( intercalate )

data AlgorithmInput =
	AlgorithmInput {
		algInput_train :: TrainingData,
		algInput_input :: Matrix
	}
	deriving( Show )

type TrainingData = NN.TrainingData

testNeuronalNetworks ::
	MonadLog m =>
		NN.NetworkDimensions
		-> (AlgorithmInput, Vector) -> m Double
testNeuronalNetworks dims =
	testWithAlg
		prettyNeuronalNetwork
		(
			NN.calcClassificationParams
				outputInterpretation
				dims
		)
		(\param -> return . NN.classify outputInterpretation param
		)
	where
		outputInterpretation = NN.outputInterpretationMaximum 10
		prettyNeuronalNetwork =
			intercalate ", " .
			map (show . Lina.size)

testWithAlg ::
	MonadLog m =>
	(param -> String)
	-> (NN.TrainingData -> m param)
	-> (param -> Matrix -> m (VectorOf Label))
	-> (AlgorithmInput, Vector) -- inputData, expected label
	-> m Double
testWithAlg prettyParam
		calcParam
		classify
		(AlgorithmInput{..}, inputLabels)
	=
	do
		doLog $ "training algorithm..."
		classificationParam <-
			calcParam $ algInput_train
		doLog $ prettyParam classificationParam 
		doLog $ "classifying test data"
		classified <-
			classify
				classificationParam
				algInput_input
		return $
			calcClassificationQuality
				(cmap round $ inputLabels) classified
		{-
		return 3
		-}
{-
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

-}

calcClassificationQuality :: VectorOf Label -> VectorOf Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

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

infoStringForParam_1D :: Gauss.ClassificationParam -> String
infoStringForParam_1D Gauss.ClassificationParam{..} =
	intercalate "\n" $
	[ concat $ ["min1:", show $ min1]
	, concat $ ["min2:", show $ min2 ]
	, concat $ ["cov1:", show $ covariance1 ]
	, concat $ ["cov2:", show $ covariance2 ]
	]
-}
