{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module NeuralNetworksTest.TestImpl(
	testNeuronalNetworks,
	TestFunctionParams(..),
	ProgressInfo, ProgressEntry,
) where

import Types
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Utils

import qualified PatternRecogn.NeuronalNetworks as NN

import Data.List( intercalate )
import qualified Data.DList as DList
import Data.DList( DList )
import Control.Monad.Random
import Control.Monad.Writer.Strict
--import qualified Data.Sequence as Seq
--import Data.Foldable as Fold


data TestFunctionParams
	= TestFunctionParams {
		loggingFreq :: Int,
		logProgressFreq :: Int,
		learningParams :: NN.LearningParams,
		stopConds :: [NN.StopCond],
		networkParams :: NN.NetworkParams
	}

type ProgressInfo = [ProgressEntry] -- the progression of success rate of the network
type ProgressEntry = R -- success rate for training data

instance (MonadLog m, Monoid w) => MonadLog (WriterT w m) where
	doLog str = lift $ doLog str

logProgressEntry e = tell $ DList.singleton e

testNeuronalNetworks ::
	forall m .
	(MonadLog m, MonadRandom m) =>
	TestFunctionParams
	-> TestData -> m ProgressInfo
testNeuronalNetworks
		TestFunctionParams{ networkParams = networkParams@NN.NetworkParams{..}, ..}
		algInput@TestData{ testData_input = mTestData }
	=
		(DList.toList <$>) $
		execWriterT $
		do
			initialState <- NN.randomPermutation $ NN.packTrainingData trainingData
			doLog $ "network dimensions: " ++ show dims
			(_, stopReason) <- NN.trainNetwork
				networkParams
				stopConds
				testWithTrainingData
				initialState
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
					do
						doLog $ intercalate "\n" $
							[ concat [ "stopped after ", show it, " iterations."]
							, concat [ "\tReason: quality >= ", show quality]
							]
						-- add last value:
						logProgressEntry $ quality 
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

		showNWInfo :: NN.ClassificationParam -> NN.TrainingMonadT NN.TrainingDataInternal (WriterT (DList ProgressEntry) m) ()
		showNWInfo network =
			NN.askIteration >>= \it ->
				when (
					loggingFreq /= 0 && (it `mod` loggingFreq) == 0
					|| 
					logProgressFreq /= 0 && (it `mod`logProgressFreq) == 0
				) $
					do
						let qualityTraining = testWithTrainingData network
						if (loggingFreq /= 0 && (it `mod` loggingFreq) == 0) then
							do
								doLog $ concat ["iteration: ", show it]
								doLog $ concat ["quality of classifying training data: ", show qualityTraining]
								maybe (return ()) `flip` mTestData $ \(testData, expectedLabels) ->
									let qualityTestData =
										testNW network
											testData
											expectedLabels
									in
										doLog $ concat ["quality of classifying test data: ", show qualityTestData]
						else
							lift $ logProgressEntry qualityTraining

		adjustWeights :: NN.ClassificationParam -> NN.TrainingMonadT NN.TrainingDataInternal (WriterT (DList ProgressEntry) m) NN.TrainingState
		adjustWeights nw =
			do
				showNWInfo nw
				--NN.randomSel 0.5 trainingData >>= \sample ->
				--let sample = Fold.toList trainingData
				NN.adjustWeightsBatch learningParams nw

		initNW = NN.initialNetworkWithRnd (NN.inputDim trainingData) dims

		trainingData :: NN.TrainingDataInternal_unpacked
		trainingData =
			NN.internalFromTrainingData outputInterpretation $
			testData_train algInput

showNW :: NN.ClassificationParam -> String
showNW =
	("network:\n" ++) .
	intercalate "\n" . map show'
	where
		show' x = show x
