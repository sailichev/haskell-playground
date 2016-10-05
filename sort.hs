module Sort(mergeSort, quickSort) where

  mergeSort :: (Ord a) => [a] -> [a]
  mergeSort [] = []
  mergeSort [single] = [single]
  mergeSort input = merge (mergeSort leftHalf) (mergeSort rightHalf)
    where
      leftHalf  = take half input
      rightHalf = drop half input
      half = div (length input) 2
      merge left []  = left
      merge [] right = right
      merge left@(leftHead:leftRest) right@(rightHead:rightRest)
        | leftHead < rightHead = leftHead  : merge leftRest right
        | otherwise            = rightHead : merge left     rightRest

  quickSort :: (Ord a) => [a] -> [a]
  quickSort [] = []
  quickSort (head:rest) = quickSort lesserElements ++ [head] ++ quickSort greaterElements
    where
      lesserElements  = filter (<=head) rest
      greaterElements = filter (>head) rest
