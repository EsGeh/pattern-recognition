{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LoadTestData where

import Types
import PatternRecogn.Types
import PatternRecogn.Lina as Lina

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char

trainingDataFormat =
	CSV.defaultDecodeOptions

inputDataFormat =
	CSV.defaultDecodeOptions{
		CSV.decDelimiter = fromIntegral (ord ' ')
	}

-----------------------------------------------------------------

-- | given the input data as a matrix the rows are filtered by a condition
prepareInputData :: (Double -> Bool) -> Matrix -> (Vector, Matrix)
prepareInputData filterRowsBy = 
	(\rawInput ->
		(
			flatten $ (rawInput ?? (All, Take 1)),
			rawInput ?? (All, Drop 1)
		)
	)
	.
	fromLists
	.
	filter (filterRowsBy . head)
	.
	toLists

loadMatrixFromFile :: CSV.DecodeOptions -> FilePath -> ErrT IO Matrix
loadMatrixFromFile fmtOpts path =
	(fromRows . Vec.toList) <$>
	(
		ExceptT $
		fmap (CSV.decodeWith fmtOpts CSV.NoHeader) $
		BS.readFile path
	)

instance CSV.FromRecord Vector where
	parseRecord v =
		fmap Lina.fromList $ CSV.parseRecord v
