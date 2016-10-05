module Search(bsearch) where

  bsearch :: (Ord a) => a -> [(a, b)] -> Maybe b
  bsearch _ [] = Nothing
  bsearch x list
    | x < keyAt middle = bsearch x up
    | x > keyAt middle = bsearch x down
    | otherwise = Just (valueAt middle)
    where
      keyAt   index = fst (list !! index)
      valueAt index = snd (list !! index)
      middle = div (length list) 2
      up     = take middle list
      down   = drop (middle + 1) list
