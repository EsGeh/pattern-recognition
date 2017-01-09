{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module NeuralNetworksTest.TestImpl(
	testNeuronalNetworks,
	TestFunctionParams(..),
) where

import Types
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Utils

import qualified PatternRecogn.NeuronalNetworks as NN

import Data.List( intercalate )
import Control.Monad.Random


data TestFunctionParams
	= TestFunctionParams {
		loggingFreq :: Int,
		learningParams :: NN.LearningParams,
		stopConds :: [NN.StopCond],
		networkParams :: NN.NetworkParams
	}

testNeuronalNetworks ::
	forall m .
	(MonadLog m, MonadRandom m) =>
	TestFunctionParams
	-> TestData -> m ()
testNeuronalNetworks
		TestFunctionParams{ networkParams = networkParams@NN.NetworkParams{..}, ..}
		algInput@TestData{ testData_input = mTestData }
	=
		do
			doLog $ "network dimensions: " ++ show dims
			(_, stopReason) <- NN.trainNetwork
				networkParams
				stopConds
				testWithTrainingData
				adjustWeights
				=<< initNW
			case stopReason of
				NN.StopReason_MaxIt it ->
					doLog $ "stopped. \n\tReason: iterations >=" ++ show it
				NN.StopReason_Converged minProgress it ->
					doLog $ intercalate "\n" $
					[ concat [ "stopped after ", show it, " iterations."]
					, concat [ "\tReason: progress < ", show minProgress]
					]
				NN.StopReason_QualityReached quality it ->
					doLog $ intercalate "\n" $
					[ concat [ "stopped after ", show it, " iterations."]
					, concat [ "\tReason: quality >= ", show quality]
					]
			return ()
	where
		testWithTrainingData nw =
			testNW nw
				(Lina.fromRows $ map fst $ trainingData)
				(Lina.fromList $ map (NN.outputToLabel outputInterpretation . snd) $ trainingData)
		testNW :: NN.ClassificationParam -> Lina.Matrix -> VectorOf Label -> R
		testNW nw inputData expectedLabels =
			let
				testClasses = NN.classify outputInterpretation nw $ inputData
			in
				calcClassificationQuality
					expectedLabels
					testClasses

		showNWInfo network =
			do
				--doLog $ showNW network
				let qualityTraining = testWithTrainingData network
				doLog $ concat ["quality of classifying training data: ", show qualityTraining]
				maybe (return ()) `flip` mTestData $ \(testData, expectedLabels) ->
					let qualityTestData =
						testNW network
							testData
							expectedLabels
					in
						doLog $ concat ["quality of classifying test data: ", show qualityTestData]

		adjustWeights :: NN.ClassificationParam -> NN.TrainingMonadT m NN.NetworkTrainingData
		adjustWeights nw =
			NN.askIteration >>= \it ->
			--NN.askLastVals >>= \lastValues -> 
				do
					when (loggingFreq /= 0 && (it `mod` loggingFreq) == 0) $
						do
							doLog $ concat ["iteration: ", show it]
							showNWInfo nw
					--runReaderT `flip` lastValues $
					NN.adjustWeightsBatch learningParams trainingData nw

		initNW = NN.initialNetworkWithRnd inputDim dims
		inputDim :: Int
		inputDim = Lina.size $ fst $ head trainingData
		trainingData :: NN.TrainingDataInternal
		trainingData =
			NN.internalFromTrainingData outputInterpretation $
			testData_train algInput

showNW :: NN.ClassificationParam -> String
showNW =
	("network:\n" ++) .
	intercalate "\n" . map show'
	where
		show' x = show x
