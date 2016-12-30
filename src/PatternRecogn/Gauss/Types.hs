{-# LANGUAGE RecordWildCards #-}
module PatternRecogn.Gauss.Types where

import PatternRecogn.Types
import Data.List( intercalate )


type Classes = [Class]
data Class
	= Class {
		class_min :: Vector,
		class_cov :: Matrix
	}
	deriving( Show )

class_prettyShow :: Class -> String
class_prettyShow Class{..} =
	intercalate "\n" $
	[ concat $ ["average: ", show class_min]
	, concat $ ["cov: ", show $ class_cov]
	]
