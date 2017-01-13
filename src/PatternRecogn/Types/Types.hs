{-# LANGUAGE TupleSections #-}
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

type TrainingDataBundled =
	[(Matrix, Label)]

type TrainingData =
	[(Vector, Label)]

newtype TrainingDataBin
	= TrainingDataBin {
		fromTrainingDataBin :: TrainingDataBundled
	}

class (Monad m) => MonadLog m where
	doLog :: String -> m ()
