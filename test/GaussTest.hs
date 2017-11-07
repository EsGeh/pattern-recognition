{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified PatternRecogn.Gauss.Classify as Gauss
import qualified AbstractTest as Test
import qualified LoadTestData as Load
import Utils( allPairs )
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
		liftIO $ putStrLn $ "testing binary classification:"
		forM_ (allPairs labels) $ \(label1, label2) ->
			runTest [label1, label2]
		liftIO $ putStrLn $ "testing classification:"
		runTest labels
	where
		labels = [3,5,7,8]
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

runTest :: [Label] -> ErrT IO ()
runTest labels =
	let
		paths = map Load.pathFromLabel labels
	in
		do
			liftIO $ putStrLn $ concat $
				["testing classification of ", intercalate ", " $ map show paths]
			testData <-
				Load.readTestInput $ paths `zip` labels

			liftIO $ putStrLn $ "testing gauss classification:"
			testGauss $ testData
			--liftIO $ putStrLn $ "testing projected gauss classification:"
			--Test.testProjectedGauss $ testData

testGauss =
	Test.testWithAlg 
		(\trainingData -> return $ Gauss.calcClassificationParams trainingData)
		(\param input ->
			return $ Gauss.classify param input)
