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
import Data.Maybe


data TestFunctionParams
	= TestFunctionParams {
		loggingFreq :: Int,
		maxIt :: Int,
		learnRate :: R,
		stopConds :: [StopCond],
		networkParams :: NetworkParams
	}

data StopCond
	= StopIfConverges R
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
	MonadLog m =>
	TestFunctionParams
	-> TestData -> m ()
testNeuronalNetworks
		TestFunctionParams{networkParams = NetworkParams{..}, ..}
		algInput@TestData{ testData_input = mTestData }
	=
	testNeuronalNetworks' $ 
			NN.toInternalTrainingData outputInterpretation $
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
						(updateNW cond)
						initNW
				initNW = NN.initialNetwork inputDim dims
				inputDim :: Int
				inputDim = Lina.size $ fst $ head trainingData

				updateNW :: 
					(NN.ClassificationParam -> IterationMonadT [NN.ClassificationParam] m (Maybe StopReason))
					-> NN.ClassificationParam
					-> IterationMonadT [NN.ClassificationParam] m (Either (NN.ClassificationParam, StopReason) NN.ClassificationParam)
				updateNW cond' network =
					do
					continue <- cond' network
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
									NN.adjustWeights learnRate trainingData nw

				cond :: NN.ClassificationParam -> IterationMonadT [NN.ClassificationParam] m (Maybe StopReason)
				cond x = withIterationCtxt $ \it previousVals ->
					cond' it previousVals x
					where
						cond' it (previousVal:_) lastVal =
							if not $ it < maxIt
							then
								return $ Just $ StopReason_MaxIt it
							else
								fmap (
									listToMaybe .
									catMaybes
								) $
								temp
							where
								temp :: m [Maybe StopReason]
								temp =
									mapM `flip` stopConds $ \stopCond ->
										case stopCond of
											StopIfConverges delta ->
												if (NN.paramsDiff lastVal previousVal <= delta)
												then
													return $ return $ StopReason_Converged delta it
												else return Nothing
											StopIfQualityReached quality ->
												if (testWithTrainingData lastVal >= quality)
												then
													do
														--doLog $ "cond quali: " ++ show (testWithTrainingData lastVal)
														return $ return $ StopReason_QualityReached quality it
												else return $ Nothing
						cond' _ _ _ = return $ Nothing

				showNWInfo network =
					do
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
