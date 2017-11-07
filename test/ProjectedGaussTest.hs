{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified PatternRecogn.Gauss.Projected as GaussProjected
import qualified AbstractTest as Test
import qualified LoadTestData as Load
import Utils( allPairs )
import Types

import PatternRecogn.Types

import Control.Monad.Random.Class
import Control.Monad.Random()
import Data.List( intercalate )


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

main :: IO ()
main =
	handleErrors $
	forM_ (allPairs labels) $ uncurry runTest
	where
		labels = [3,5,7,8]
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

runTest :: Label -> Label -> ErrT IO ()
runTest label1 label2 =
	let
		paths@[path1,path2] = map Load.pathFromLabel labels
		labels = [label1, label2]
	in
		do
			liftIO $ putStrLn $ concat $
				["testing classification of ", intercalate ", " $ map show paths]
			testData <- Load.readTestInputBin path1 path2 label1 label2

			liftIO $ putStrLn $ "testing projected gauss classification:"
			testProjectedGauss $ testData

testProjectedGauss ::
	forall m .
	(MonadIO m, MonadRandom m) =>
	TestDataBin -> m ()
testProjectedGauss =
	Test.testWithAlgBin
		GaussProjected.calcClassificationParamsWithRnd
		(\param input -> return $ GaussProjected.classifyProjected param input)
