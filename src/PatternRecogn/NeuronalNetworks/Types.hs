{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.NeuronalNetworks.Types where

import PatternRecogn.Lina as Lina hiding( cond )
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils
import Control.Monad


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

type LearningParams = Either LearningParamsDefault LearningParamsSilvaAlmeida

data LearningParamsDefault
	= LearningParamsDefault {
		learnRate :: R,
		dampingFactor :: R
	}
defLearningParamsDefault =
	LearningParamsDefault {
		learnRate = 0.1,
		dampingFactor = 0
	}

data LearningParamsSilvaAlmeida
	= LearningParamsSilvaAlmeida {
		{-
		stepMin :: Maybe R,
		stepMax :: Maybe R,
		-}
		accelerateFactor :: R,
		decelerateFactor :: R
	}
defLearningParamsSilvaAlmeida =
	LearningParamsSilvaAlmeida {
		{-
		stepMin = 0.1,
		stepMax = 10,
		-}
		accelerateFactor = 2,
		decelerateFactor = 0.5
	}

type NetworkDimensions = [Int]

type TrainingDataInternal =
	[(Vector,Vector)] -- sample, expected output

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
					--if output >= 0.5 then 1 else 0
				_ -> error "outputInterpretation: error!",
		labelToOutput =
			\lbl -> Lina.fromList [fromIntegral lbl]
	}

data NetworkTrainingData =
	NetworkTrainingData {
		nwData_weights :: ClassificationParam,
		nwData_gradients :: [Matrix],
		nwData_stepWidths :: [Matrix]
	}

initialTrainingData weights =
	NetworkTrainingData{
		nwData_weights = weights,
		nwData_gradients =
			map (\x -> Lina.konst 0 $ Lina.size x) weights,
		nwData_stepWidths =
			map (Lina.konst 0.1 . Lina.size) weights
	}

-- | sums up element wise differences
paramsDiff newWeights weights =
	sum $
	map (sum . join . toLists . cmap abs) $
	zipWith (-) newWeights weights
