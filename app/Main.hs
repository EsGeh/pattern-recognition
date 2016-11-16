{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Main where

import PatternRecogn.ExpectationMax
import Plot
import Types

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char

trainingDataFormat =
	CSV.defaultDecodeOptions
trainingDataPath = "resource/2d-em.csv"
{-
inputDataFormat =
	CSV.defaultDecodeOptions{
		CSV.decDelimiter = fromIntegral (ord ' ')
	}
-}


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

main :: IO ()
main =
	handleErrors $
	do
		trainingData <- readData trainingDataFormat trainingDataPath
		mapM_
			(flip runOnce trainingData)
			classCountList
	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr
		classCountList = [1..2]

runOnce count trainingData =
	do
		liftIO $ putStrLn $ startIterationInfo
		let param =
			calcClassificationParams count trainingData
		liftIO $ putStrLn $
			concat $
			[ descriptionString trainingData param
			, "plotting..."
			]
		plot plotPath trainingData param
	where
		startIterationInfo =
			concat $
			[ "----------------------------------------------\n"
			, "running EM algorithm with "
			, show count
			, " classes..."
			]
		plotPath = concat $ ["plots/", show count, ".svg"]

readData :: CSV.DecodeOptions -> FilePath -> ErrT IO Matrix
readData fmtOpts path =
	(fromRows . Vec.toList) <$>
	(
		ExceptT $
		fmap (CSV.decodeWith fmtOpts CSV.NoHeader) $
		BS.readFile path
	)

-- (helpers: )
-----------------------------------------------------------------

descriptionString set1 param =
	unlines $
	[ concat $ ["set1 size:", show $ size set1]
	, infoStringForParam param
	]

-----------------------------------------------------------------
-- utils:
-----------------------------------------------------------------

instance CSV.FromRecord Vector where
	parseRecord v =
		fmap fromList $ CSV.parseRecord v

-- return all pairs in a list
allPairs l =
	case l of
		(x:rest) ->
			map (x,) rest ++ allPairs rest
		_ -> []

uncurry4 f (a,b,c,d) = f a b c d
