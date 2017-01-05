{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module NeuralNetworksTest.TestImpl where

import Utils
import Types
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Utils

import qualified PatternRecogn.NeuronalNetworks as NN

import Data.List( intercalate )
import Control.Monad.Random
import Data.Maybe


data TestFunctionParams
	= TestFunctionParams {
		loggingFreq :: Int,
		--maxIt :: Int,
		learnRate :: R,
		stopConds :: [StopCond],
		networkParams :: NetworkParams
	}

data StopCond
	= StopAfterMaxIt Int
	| StopIfConverges R
	| StopIfQualityReached R

data StopReason
	= StopReason_MaxIt Int
	| StopReason_Converged R Int
	| StopReason_QualityReached R Int

data NetworkParams
	= NetworkParams {
		dims :: NN.NetworkDimensions,
		outputInterpretation :: NN.OutputInterpretation
	}

testNeuronalNetworks ::
	forall m .
	(MonadLog m, MonadRandom m) =>
	TestFunctionParams
	-> TestData -> m ()
testNeuronalNetworks
		TestFunctionParams{networkParams = NetworkParams{..}, ..}
		algInput@TestData{ testData_input = mTestData }
	=
	testNeuronalNetworks' $ 
			NN.internalFromTrainingData outputInterpretation $
			testData_train algInput
	where
		testNeuronalNetworks' :: NN.TrainingDataInternal -> m ()
		testNeuronalNetworks' trainingData =
			do
				doLog $ "network dimensions: " ++ show dims
				(_, stopReason) <- trainNetwork
				case stopReason of
					StopReason_MaxIt it ->
						doLog $ "stopped. \n\tReason: iterations >=" ++ show it
					StopReason_Converged minProgress it ->
						doLog $ intercalate "\n" $
						[ concat [ "stopped after ", show it, " iterations."]
						, concat [ "\tReason: progress < ", show minProgress]
						]
					StopReason_QualityReached quality it ->
						doLog $ intercalate "\n" $
						[ concat [ "stopped after ", show it, " iterations."]
						, concat [ "\tReason: quality >= ", show quality]
						]
				return ()
			where
				trainNetwork :: m (NN.ClassificationParam, StopReason)
				trainNetwork =
					iterateWithCtxtM 1
						updateNW
						=<<
						initNW
				initNW = NN.initialNetworkWithRnd inputDim dims
				inputDim :: Int
				inputDim = Lina.size $ fst $ head trainingData

				updateNW :: 
					NN.ClassificationParam
					-> IterationMonadT [NN.ClassificationParam] m (Either (NN.ClassificationParam, StopReason) NN.ClassificationParam)
				updateNW network =
					do
					continue <- cond network
					case continue of
						Nothing ->
							do
								ret <- updateNW' network
								return $ Right ret
						Just stop -> return $ Left (network, stop)
					where
						updateNW' :: 
							NN.ClassificationParam
							-> IterationMonadT [NN.ClassificationParam] m NN.ClassificationParam
						updateNW' nw =
							withIterationCtxt $ \it _ ->
								do
									when (loggingFreq /= 0 && (it `mod` loggingFreq) == 0) $
										do
											doLog $ concat ["iteration: ", show it]
											showNWInfo nw
									NN.adjustWeightsBatchWithRnd learnRate trainingData nw

				cond :: NN.ClassificationParam -> IterationMonadT [NN.ClassificationParam] m (Maybe StopReason)
				cond x = withIterationCtxt $ \it previousVals ->
					cond' it previousVals x
					where
						cond' it (previousVal:_) lastVal =
							{-
							if not $ it < maxIt
							then
								return $ Just $ StopReason_MaxIt it
							else
							-}
								fmap (
									listToMaybe .
									catMaybes
								) $
								temp
							where
								temp :: m [Maybe StopReason]
								temp =
									mapM `flip` stopConds $ \stopCond ->
										return $
										case stopCond of
											StopAfterMaxIt maxIt ->
												if not $ it < maxIt then return $ StopReason_MaxIt it else Nothing
											StopIfConverges delta ->
												if (NN.paramsDiff lastVal previousVal <= delta)
												then
													return $ StopReason_Converged delta it
												else Nothing
											StopIfQualityReached quality ->
												if (testWithTrainingData lastVal >= quality)
												then
													do
														--doLog $ "cond quali: " ++ show (testWithTrainingData lastVal)
														return $ StopReason_QualityReached quality it
												else Nothing
						cond' _ _ _ = return $ Nothing

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

				testWithTrainingData nw =
					testNW nw
						(Lina.fromRows $ map fst $ trainingData)
						(Lina.fromList $ map (NN.outputToLabel outputInterpretation . snd) $ trainingData)
				testNW nw inputData expectedLabels =
					let
						testClasses = NN.classify outputInterpretation nw $ inputData
					in
						calcClassificationQuality
							expectedLabels
							testClasses

showNW :: NN.ClassificationParam -> String
showNW =
	("network:\n" ++) .
	intercalate "\n" . map show'
	where
		show' x = show x
