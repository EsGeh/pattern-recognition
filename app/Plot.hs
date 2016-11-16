{-# LANGUAGE RecordWildCards #-}
module Plot where

import Types
import PatternRecogn.ExpectationMax

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )

import Graphics.Rendering.Chart as Chart hiding( Matrix )
import Graphics.Rendering.Chart.Backend.Diagrams as Chart
import Data.Default.Class

import Control.Monad.Trans
import Control.Lens


plot :: FilePath -> Matrix -> ClassificationParam -> ErrT IO ()
plot path dots params =
	do
		_ <- lift $ Chart.renderableToFile def path $ Chart.toRenderable diagram
		return ()
	where
		diagram =
			Chart.layout_title .~ "title" $
			Chart.layout_plots .~
				([ toPlot $ pointsPlot
				]
				++
				classPlot
				) $
			def
		pointsPlot =
			plot_points_values .~ points $
			def
			where
				points :: [(Double, Double)]
				points =
					map vecToTuple $
					Lina.toRows dots
		classPlot =
			map (\p -> singleEllipse p `joinPlot` singleCenterDot p)
				params
		singleEllipse Class{..} =
			toPlot $
			plot_points_style .~ style $
			plot_points_values .~ [vecToTuple class_min]$
			def
			where
				style =
					point_radius .~ 1 $
					def
		singleCenterDot Class{..} =
			toPlot $
			plot_points_style .~ style $
			plot_points_values .~ [vecToTuple class_min]$
			def
			where
				style =
					def


vecToTuple = listToTuple . Lina.toList
	where
		listToTuple [x,y] = (x,y)
		listToTuple _ = error "error extracting dots"
