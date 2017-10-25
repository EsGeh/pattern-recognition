{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AbstractTest where

import Types
import Utils
import PatternRecogn.Types

import qualified PatternRecogn.Lina as Lina
import PatternRecogn.Utils
import Data.List


testWithAlgBin ::
	MonadIO m =>
	(TrainingDataBin -> m param)
	-> (param -> Matrix -> m (VectorOf Label))
	-> TestDataBin
	-> m ()
testWithAlgBin =
	testWithAlg' calcTestDataFromTrainingData
	where
		calcTestDataFromTrainingData =
			calcTestDataFromBundledTrainingData . fromTrainingDataBin

testWithAlg ::
	MonadIO m =>
	(TrainingDataBundled -> m param)
	-> (param -> Matrix -> m (VectorOf Label))
	-> TestDataBundled
	-> m ()
testWithAlg =
	testWithAlg' calcTestDataFromBundledTrainingData

testWithAlg' ::
	MonadIO m =>
	(trainingData -> (Matrix, VectorOf Label))
	-> (trainingData -> m param)
	-> (param -> Matrix -> m (VectorOf Label))
	-> TestDataGen trainingData
	-> m ()
testWithAlg'
		calcTestDataFromTrainingData
		calcParam
		classify
		TestData{ testData_input = mInput, .. }
	=
	do
		classificationParam <-
			calcParam testData_train
		let (testData_training, expectedLabels_training) = calcTestDataFromTrainingData testData_train
		classified_training <-
			classify
				classificationParam
				testData_training
		let successRate_training =
			calcClassificationQuality expectedLabels_training classified_training
		liftIO $ putStrLn $ concat $
			[ "training data quality: ", show successRate_training]
		liftIO $ putStrLn $ concat $
			[ "confusion matrix:\n"
			, dispf 2 $ confusionMatr expectedLabels_training classified_training
			]
		maybe (return ()) `flip` mInput $ \(inputData, expectedLabels) ->
			do
				classified <-
					classify
						classificationParam
						inputData
				let quality = calcClassificationQuality expectedLabels classified
				liftIO $ putStrLn $ concat $
					[ "test data quality: ", show quality ]
				liftIO $ putStrLn $ concat $
					[ "confusion matrix:\n"
					, dispf 2 $ confusionMatr expectedLabels_training classified_training
					]

-- TrainingDataBundled = [(Matrix, Label)]

calcTestDataFromBundledTrainingData :: TrainingDataBundled -> (Matrix, VectorOf Label)
calcTestDataFromBundledTrainingData trainingData =
	let
		temp = map (\(m, label) -> (m, replicate (Lina.rows m) label)) trainingData
		(matrices, labels) = (map fst temp, map snd temp)
	in
		(foldl1 (Lina.===) matrices, Lina.fromList $ join $ labels)

confusionMatr :: VectorOf Label -> VectorOf Label -> Matrix
confusionMatr expected res =
	build (dims, dims) $ \(row :: Double) (col :: Double) ->
		(/ fromIntegral (size res)) $
		fromIntegral $ length $
		filter (\(correctLabel, resultLabel) -> correctLabel == floor col && resultLabel == floor row) $
		testResultList
	where
		testResultList :: [(Label, Label)]
		testResultList = toList expected `zip` toList res
		dims = length allLabels
		allLabels =
			nub $ toList expected
