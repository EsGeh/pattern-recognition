{-# LANGUAGE RecordWildCards #-}
module Plot where

import Types

--import PatternRecogn.Utils
import PatternRecogn.Lina as Lina

import Graphics.Rendering.Chart.Easy as Chart hiding( Matrix, Vector )
import Graphics.Rendering.Chart.Backend.Diagrams as Chart

--import Control.Lens

plotProgresses :: Int -> FilePath -> [(String, [[R]])] -> ErrT IO ()
plotProgresses measureFreq path plotParams =
	do
		_ <- lift $ Chart.renderableToFile def{ _fo_format = Chart.SVG } path $ Chart.toRenderable diagram
		return ()
	where
		diagram :: EC (Layout Double Double) ()
		diagram =
			do
				layout_title .= path
				forM_ plotParams $ \(title, progresses) ->
					Chart.plot $ line title $
						-- map `flip` lines $
						map ((fromIntegral <$> ([0,measureFreq ..] :: [Int])) `zip`) $
						progresses

plotProgress :: FilePath -> String -> [R] -> ErrT IO ()
plotProgress path title successRates =
	do
		_ <- lift $ Chart.renderableToFile def path $ Chart.toRenderable diagram
		return ()
	where
		diagram :: EC (Layout Double Double) ()
		diagram =
			do
				layout_title .= path
				Chart.plot $ line title $ [
					((fromIntegral <$> ([0..] :: [Int])) `zip` successRates)
					]
