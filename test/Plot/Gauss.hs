{-# LANGUAGE RecordWildCards #-}
module Plot.Gauss where

import Types

import PatternRecogn.Gauss.Utils
import PatternRecogn.Utils
import PatternRecogn.Lina as Lina
import PatternRecogn.Gauss.Types

import Graphics.Rendering.Chart.Easy as Chart hiding( Matrix, Vector )
import Graphics.Rendering.Chart.Backend.Diagrams as Chart


-- plot 1-dim data and estimated propability measure (gaussian)
plotProjected :: FilePath -> Vector -> Classes -> ErrT IO ()
plotProjected path dots params =
	do
		_ <- lift $ Chart.renderableToFile def path $ Chart.toRenderable diagram
		return ()
	where
		diagram :: EC (Layout Double Double) ()
		diagram =
			do
				layout_title .= path
				Chart.plot $ points "data points" $
					map (\x -> (x, 0)) $
					Lina.toList dots
				Chart.plot $ line "estimated distribution" $
					concat $ map (\Class{..} -> gaussLine (vecToVal class_min) (matToVal class_cov)) $
					params

vecToVal :: Vector -> R
vecToVal =
	f
	.
	Lina.toList
	where
		f [x] = x
		f _ = error "error converting to vector to scalar!"

matToVal :: Matrix -> R
matToVal =
	f
	.
	Lina.toLists
	where
		f [[x]] = x
		f _ = error "error converting to vector to scalar!"

gaussLine :: R -> R -> [[(R,R)]]
gaussLine center cov =
	return $
	map (\x -> (x, mahalanobis (scalar center) (scalar cov) (scalar x))) $
	map (
		\x -> (center - width / 2) + (x * width / count)
	) $
	[0..count]
	where
		count = 100 :: Num a => a
		width = 30 * cov

plot :: FilePath -> Matrix -> Classes -> ErrT IO ()
plot path dots params =
	do
		_ <- lift $ Chart.renderableToFile def path $ Chart.toRenderable diagram
		return ()
	where
		diagram :: EC (Layout Double Double) ()
		diagram =
			do
				layout_title .= path
				Chart.plot $ points "data points" $
					map vecToTuple2 $
					Lina.toRows dots
				Chart.plot $
					line "classes" $
						map (
							\Class{..} ->
								map vecToTuple2 $
								lineFromGauss class_min class_cov
						)
						params

lineFromGauss :: Vector -> Matrix -> [Vector]
lineFromGauss center cov =
	toRows $
	(+ asRow center) $ -- shift to center
	(<> cov) $ 			-- multiply with covariance matrix
	circleDots 
	where
		circleDots =
			cmap cos anglesMat
			|||
			cmap sin anglesMat
		anglesMat = 
			((count :: Int)><1) angles
		angles = (/(2*pi)) <$> ([0..(count-1)] :: [Double])
		count :: Num a => a
		count = 100

{-
vectorField :: String -> [((R,R),(R,R))] -> EC (Layout R R) (Plot R R)
vectorField title vectors =
	fmap plotVectorField $ liftEC $
	do
		c <- takeColor
		plot_vectors_values .= vectors
		plot_vectors_style . vector_line_style . line_color .= c
		plot_vectors_style . vector_head_style . point_color .= c
		plot_vectors_title .= title
		----plot_vectors_grid .= grid
-}
