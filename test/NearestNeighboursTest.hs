{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified PatternRecogn.NearestNeighbours as NearestNeighbours
import qualified AbstractTest as Test
import qualified LoadTestData as Load
import Types

import PatternRecogn.Types

import Control.Monad.Random()
import Data.List( intercalate )


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

main :: IO ()
main =
	handleErrors $
	do
		liftIO $ putStrLn "loading all data from file..."
		testData <- loadData
		liftIO $ putStrLn "testing nearest neighbour classification..."
		forM_ [1..5] $ \k ->
			do
			liftIO $ putStrLn $ concat [ "k = ", show k ]
			testAlgorithm testData k
	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

loadData =
	Load.readTestInput $
	map (\digit -> (Load.pathFromLabel digit, digit))
	[0..9]

testAlgorithm :: TestDataBundled -> Int -> ErrT IO ()
testAlgorithm testData k =
	Test.testWithAlg 
		(return . NearestNeighbours.calcClassificationParams . fromBundled)
		(\param -> return . NearestNeighbours.classify k param)
		testData
