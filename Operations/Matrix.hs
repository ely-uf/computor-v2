module Operations.Matrix
  ( takeRow
  , takeCol
  ) where

import Types

takeRow :: Matrix a -> Int -> Maybe [a]
takeRow (Matrix nr nc lst) row
  | row > nr || row <= 0 = Nothing
  | otherwise = Just $ take nc $ drop (nc * (row - 1)) lst

takeCol :: Matrix a -> Int -> Maybe [a]
takeCol (Matrix nr nc lst) col
  | col > nc || col <= 0 = Nothing
  | otherwise = Just $ take nr $ takeEveryXFrom lst nr col

takeEveryXFrom :: [a] -> Int -> Int -> [a]
takeEveryXFrom [] _ _ = []
takeEveryXFrom _  a _ | a <= 0 = []
takeEveryXFrom lst num from = last (take from lst) : takeEveryX (drop from lst) num

takeEveryX :: [a] -> Int -> [a]
takeEveryX [] _ = []
takeEveryX _  a | a <= 0 = []
takeEveryX lst a = last (take a lst) : takeEveryX (drop a lst) a
