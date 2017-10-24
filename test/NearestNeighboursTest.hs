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
	testAlgorithm
	--forM_ (allPairs [3,5,7,8]) $ uncurry testGauss
	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

testAlgorithm :: ErrT IO ()
testAlgorithm =
	do
		liftIO $ putStrLn "loading all data from file..."
		testData <-
			Load.readTestInput $
			map (\digit -> (Load.pathFromLabel digit, digit))
			[0..9]
		liftIO $ putStrLn "testing nearest neighbour classification..."
		Test.testWithAlg 
			(return . NearestNeighbours.calcClassificationParams . fromBundled)
			(\param -> return . NearestNeighbours.classify param)
			testData
