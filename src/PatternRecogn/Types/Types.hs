module PatternRecogn.Types.Types(
	MonadLog(..),
	Label,
	TrainingDataBundled,
	TrainingData,
	TrainingDataBin(..), 
) where

import PatternRecogn.Lina as Lina
--import Control.Monad.IO.Class

import Foreign.C.Types( CInt )


type Label = CInt

-- |for every label:
-- a matrix where the rows are all feature vectors for that label
type TrainingDataBundled =
	[(Matrix, Label)]

-- |feature vector + label
type TrainingData =
	[(Vector, Label)]

-- |feature vectors sorted into 2 labels
newtype TrainingDataBin
	= TrainingDataBin {
		fromTrainingDataBin :: TrainingDataBundled
	}

class (Monad m) => MonadLog m where
	doLog :: String -> m ()
