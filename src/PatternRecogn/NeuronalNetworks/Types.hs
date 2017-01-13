{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.NeuronalNetworks.Types where

import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils

import Data.Foldable as Fold
import Control.Monad.Random
import Control.Monad
import qualified Data.Sequence as Seq
--import Control.DeepSeq


-----------------------------------------------------------------
-- Types:
-----------------------------------------------------------------

-- |represents the whole network
-- |list of weight-matrix for every layer
-- |	columns: weights for one perceptron
type ClassificationParam
	= [Matrix]

data NetworkParams
	= NetworkParams {
		dims :: NetworkDimensions,
		outputInterpretation :: OutputInterpretation
	}

data LearningParams
	= LearningParams {
		learningP_specificParams :: SpecificLearningParams,
		learningP_sampleSize :: Maybe Int
	}

data SpecificLearningParams
	= LearningParamsDefault DefaultLearningParams
	| LearningParamsSilvaAlmeida SilvaAlmeidaParams
	| LearningParamsRProp RPropParams

data DefaultLearningParams
	= DefaultLearningParams {
		learnRate :: R,
		dampingFactor :: R
	}
defDefaultLearningParams =
	DefaultLearningParams {
		learnRate = 0.1,
		dampingFactor = 0
	}

data SilvaAlmeidaParams
	= SilvaAlmeidaParams {
		silva_accelerateFactor :: R,
		silva_decelerateFactor :: R
	}
defSilvaAlmeidaParams =
	SilvaAlmeidaParams {
		silva_accelerateFactor = 2,
		silva_decelerateFactor = 0.5
	}

data RPropParams
	= RPropParams {
		rprop_stepMin :: R,
		rprop_stepMax :: R,
		rprop_accelerateFactor :: R,
		rprop_decelerateFactor :: R
	}
defRPropParams =
	RPropParams {
		rprop_stepMin = 0.1,
		rprop_stepMax = 10,
		rprop_accelerateFactor = 2,
		rprop_decelerateFactor = 0.5
	}

type NetworkDimensions = [Int]

type TrainingDataInternal_unpacked =
	[(Vector,Vector)] -- sample, expected output

type TrainingDataInternal =
	Seq.Seq (Vector,Vector) -- sample, expected output

randomPermutation :: MonadRandom m => TrainingDataInternal -> m TrainingDataInternal
randomPermutation l
	| Seq.null l = return l
	| otherwise =
		do
			index <-
				getRandomR (0, Seq.length l - 1)
			(\rest -> ((Seq.<|) $ ((Seq.index $ l) $ index)) $ rest) <$> randomPermutation (deleteAt index $ l)
			--(\rest -> ((Seq.<|) $!! ((Seq.index $!! l) $!! index)) $!! rest) <$> randomPermutation (force $ deleteAt index $!! l)

deleteAt index l =
	case Seq.splitAt index l of
		(x, y) -> x Seq.>< Seq.drop 1 y

rotateTrainingData :: Int -> TrainingDataInternal -> TrainingDataInternal
rotateTrainingData index =
	(\(x,y) -> y Seq.>< x)
	.
	Seq.splitAt index

takeSample :: Int -> TrainingDataInternal -> TrainingDataInternal_unpacked
takeSample sampleMaxSize l =
	Fold.toList $ Seq.take sampleMaxSize l

packTrainingData :: TrainingDataInternal_unpacked -> TrainingDataInternal
packTrainingData = Seq.fromList

unpackTrainingData :: TrainingDataInternal -> [(Vector,Vector)]
unpackTrainingData = Fold.toList

internalFromTrainingData OutputInterpretation{..} =
	map (mapToSnd $ labelToOutput)

internalFromBundledTrainingData OutputInterpretation{..} =
	map (mapToSnd $ labelToOutput)
	.
	fromBundled

data OutputInterpretation =
	OutputInterpretation {
		outputToLabel :: Vector -> Label,
		labelToOutput :: Label -> Vector
	}

data StopCond
	= StopAfterMaxIt Int
	| StopIfConverges R
	| StopIfQualityReached R

data StopReason
	= StopReason_MaxIt Int
	| StopReason_Converged R Int
	| StopReason_QualityReached R Int

-- | 0 <-> (1,0,0,...); 3 <-> (0,0,0,1,0,...)
outputInterpretationMaximum count =
	OutputInterpretation{
		outputToLabel =
			fromIntegral . Lina.maxIndex,
		labelToOutput =
			\lbl -> Lina.fromList $
				setElemAt (fromIntegral lbl) 1 $ replicate count 0
	}

outputInterpretationSingleOutput =
	OutputInterpretation{
		outputToLabel = \x ->
			case Lina.toList x of
				[output] ->
					round output
				_ -> error "outputInterpretation: error!",
		labelToOutput =
			\lbl -> Lina.fromList [fromIntegral lbl]
	}

data TrainingState =
	TrainingState {
		nwData_weights :: ClassificationParam,
		nwData_gradients :: [Matrix],
		nwData_stepWidths :: [Matrix]
		--nwData_trainingView :: TrainingDataInternal
	}

initialTrainingState weights =
	TrainingState{
		nwData_weights = weights,
		nwData_gradients =
			map (\x -> Lina.konst 0 $ Lina.size x) weights,
		nwData_stepWidths =
			map (Lina.konst 0.1 . Lina.size) weights
		--nwData_trainingView = trainingData
	}

-- | sums up element wise differences
paramsDiff newWeights weights =
	sum $
	map (sum . join . toLists . cmap abs) $
	zipWith (-) newWeights weights
