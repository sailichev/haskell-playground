import Sort;
import Search;

searchTest search =
    search 1 [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (7, 7), (8, 8)] == Just 1  &&
    search 8 [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (7, 7), (8, 8)] == Just 8  &&
    search 4 [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (7, 7), (8, 8)] == Just 4  &&
    search 1 [(1, 1)]                                                 == Just 1  &&
    search 0 [(1, 1)]                                                 == Nothing &&
    search 6 [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (7, 7), (8, 8)] == Nothing &&
    search 1 []                                                       == Nothing

sortTest sort =
    (sort [1, 9, 3, 2, 5, 4, 6, 8, 7]) !! 0 == 1 &&
    (sort [1, 9, 3, 2, 5, 4, 6, 8, 7]) !! 8 == 9 &&
    (sort [1, 9, 3, 2, 5, 4, 6, 8, 7]) !! 4 == 5 &&
    (sort [3, 1]) == [1, 3] &&
    (sort [1]) == [1] &&
    (sort []) == []

testResut :: String -> Bool -> String
testResut name test =
    name ++ " : " ++ if test then "passed" else "failed"

main :: IO()
main = do
    print (testResut "bsearch"   (searchTest bsearch  ))
    print (testResut "mergeSort" (sortTest   mergeSort))
    print (testResut "quickSort" (sortTest   quickSort))
