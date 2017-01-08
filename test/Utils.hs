{-# LANGUAGE TupleSections #-}
module Utils where

--import PatternRecogn.Types


-- return all pairs in a list
allPairs l =
	case l of
		(x:rest) ->
			map (x,) rest ++ allPairs rest
		_ -> []
