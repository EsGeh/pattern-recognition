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

data AlgorithmInput =
	AlgorithmInput {
		algInput_train :: TrainingData,
		algInput_input :: (Matrix, VectorOf Label) -- inputData, expected label
	}
	deriving( Show )

type TrainingData = NN.TrainingData

testNeuronalNetworks ::
	forall m .
	MonadLog m =>
	NN.NetworkDimensions
	-> AlgorithmInput -> m ()
testNeuronalNetworks dims algInput@AlgorithmInput{ algInput_input = (testData, expectedLabels) } =
	let
		learnRate = 1
	in
		do
			_ <- train learnRate $
				NN.toInternalTrainingData outputInterpretation $
				algInput_train algInput
			return ()
	where
		outputInterpretation = NN.outputInterpretationMaximum 10
		train :: R -> NN.TrainingDataInternal -> m NN.ClassificationParam
		train learnRate trainingData =
			let
				initNW = NN.initialNetwork inputDim dims
				inputDim :: Int
				inputDim = 
					Lina.size $ fst $ head trainingData
			in
				(last <$>) $
				iterateWhileM
					cond
					(updateNW learnRate)
					initNW
				where
					cond it (lastVal:previousVal:_) =
						NN.paramsDiff lastVal previousVal >= 0.1
					cond _ _ = True
					updateNW :: R -> Int -> NN.ClassificationParam -> m NN.ClassificationParam
					updateNW learnRate it oldNW =
						do
							newNW <- NN.adjustWeights learnRate trainingData oldNW :: m NN.ClassificationParam
							when ((it `mod` 10) == 0) $
								do
									doLog $ unlines $
										[ concat ["iteration: ", show it]
										]
									testWithTrainingData newNW
									testWithInput newNW
							return $ newNW
						where
							testWithTrainingData nw =
									do
										let testClasses = NN.classify outputInterpretation nw $ Lina.fromRows $ map fst $ trainingData :: VectorOf Label
										let quality =
											calcClassificationQuality
											(Lina.fromList $ map (NN.outputToLabel outputInterpretation . snd) $ trainingData)
												testClasses
										doLog $ concat ["quality of classifying training data: ", show quality]
							testWithInput nw =
									do
										let testClasses = NN.classify outputInterpretation nw $ testData
										let quality =
											calcClassificationQuality
												expectedLabels
												testClasses
										doLog $ concat ["quality of classifying input data: ", show quality]

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
