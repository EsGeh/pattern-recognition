{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module TestMultiple where

import Types
import Plot
import PatternRecogn.Types
import PatternRecogn.Lina as Lina
import PatternRecogn.Utils

import qualified PatternRecogn.NeuronalNetworks as NN

import Control.Monad.Random as Rand
import Control.Monad.State
import Data.List( intercalate )
import Data.Maybe

data AlgorithmInput =
	AlgorithmInput {
		algInput_train :: TrainingData,
		algInput_input :: Maybe (Matrix, VectorOf Label) -- inputData, expected label
	}
	deriving( Show )

type TrainingData = NN.TrainingData

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
	| StopReason_Converged R
	| StopReason_QualityReached R

data NetworkParams
	= NetworkParams {
		dims :: NN.NetworkDimensions,
		outputInterpretation :: NN.OutputInterpretation
	}

testNeuronalNetworks ::
	forall m .
	MonadLog m =>
	TestFunctionParams
	-> AlgorithmInput -> m ()
testNeuronalNetworks
		TestFunctionParams{networkParams = NetworkParams{..}, ..}
		algInput@AlgorithmInput{ algInput_input = mTestData }
	=
	testNeuronalNetworks' $ 
			NN.toInternalTrainingData outputInterpretation $
			algInput_train algInput
	where
		testNeuronalNetworks' :: NN.TrainingDataInternal -> m ()
		testNeuronalNetworks' trainingData =
			do
				doLog $ "network dimensions: " ++ show dims
				(nw,stopReason) <- train
				case stopReason of
					StopReason_MaxIt it -> doLog $ "max iterations reached " ++ show it
					StopReason_Converged minProgress -> doLog $ "stopped: progress < " ++ show minProgress
					StopReason_QualityReached quality -> doLog $ "stopped: quality >= " ++ show quality
				_ <- showNWInfo =<<
					(NN.adjustWeights learnRate trainingData $ last nw)
				return ()
			where
				train :: m ([NN.ClassificationParam],StopReason)
				train =
					--(last <$>) $
					iterateWhileM_ext
						cond
						updateNW
						initNW
				initNW = NN.initialNetwork inputDim dims
				inputDim :: Int
				inputDim = Lina.size $ fst $ head trainingData
				cond :: Int -> [NN.ClassificationParam] -> m (Maybe StopReason)
				cond it (lastVal:previousVal:_) =
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
											return $ return $ StopReason_Converged delta
										else return Nothing
									StopIfQualityReached quality ->
										if (testWithTrainingData lastVal >= quality)
										then
											do
												--doLog $ "cond quali: " ++ show (testWithTrainingData lastVal)
												return $ return $ StopReason_QualityReached quality
										else return $ Nothing
				cond _ _ = return $ Nothing
				updateNW :: Int -> NN.ClassificationParam -> m NN.ClassificationParam
				updateNW it network =
					do
						when (loggingFreq /= 0 && (it `mod` loggingFreq) == 0) $
							do
								doLog $ concat ["iteration: ", show it]
								showNWInfo network
						ret <- NN.adjustWeights learnRate trainingData network
						return ret
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
