module Operations.Matrix
  ( matrixMultiplication
  , takeRow
  , takeCol
  ) where

import Types
import Debug.Trace (trace)
import Data.Maybe (isNothing, fromJust)

matrixMultiplication :: (Num a, Show a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixMultiplication m1@(Matrix nr1 nc1 _) m2@(Matrix nr2 nc2 _)
  | nc1 /= nr2 = Nothing
  | otherwise = if any isNothing multipliedMatrix
                  then Nothing
                  else Just $ Matrix nr1 nc2 (map fromJust multipliedMatrix)
  where multipliedMatrix = [ cellVal i k | i <- [1 .. nr1], k <- [1.. nc2] ]
        cellVal i k = do
          m1Row <- takeRow m1 i
          m2Col <- takeCol m2 k
          trace ("[" ++ show i ++ "|" ++ show k ++"] = " ++ show m1Row ++ " ** " ++ show m2Col) (Just ())
          return . sum $ zipWith (*) m1Row m2Col

takeRow :: Matrix a -> Int -> Maybe [a]
takeRow (Matrix nr nc lst) row
  | row > nr || row <= 0 = Nothing
  | otherwise = Just $ take nc $ drop (nc * (row - 1)) lst

takeCol :: Matrix a -> Int -> Maybe [a]
takeCol (Matrix nr nc lst) col
  | col > nc || col <= 0 = Nothing
  | otherwise = Just $ takeEveryXInCol lst col nc

takeEveryXInCol :: [a] -> Int -> Int -> [a]
takeEveryXInCol [] _ _ = []
takeEveryXInCol _  a _ | a <= 0 = []
takeEveryXInCol a el _ | length a < el = []
takeEveryXInCol lst el rowLen = lst !! (el - 1) : takeEveryXInCol (drop rowLen lst) el rowLen
