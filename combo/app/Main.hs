module Main where

-- QuickSort implementation
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger = quicksort [a | a <- xs, a > x]
   in smaller ++ [x] ++ bigger

-- Binary Search implementation
binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch xs target = binarySearchHelper xs target 0 (length xs - 1)
  where
    binarySearchHelper :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
    binarySearchHelper xs target left right
      | left > right = Nothing
      | xs !! mid == target = Just mid
      | xs !! mid > target = binarySearchHelper xs target left (mid - 1)
      | otherwise = binarySearchHelper xs target (mid + 1) right
      where
        mid = (left + right) `div` 2

-- Fibonacci with memoization using lazy evaluation
fibonacci :: Int -> Integer
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Test function to demonstrate algorithms
testAlgorithms :: IO ()
testAlgorithms = do
  let unsortedList = [64, 34, 25, 12, 22, 11, 90]
  putStrLn "Original list:"
  print unsortedList

  putStrLn "\nSorted list using QuickSort:"
  let sortedList = quicksort unsortedList
  print sortedList

  putStrLn "\nBinary Search test:"
  let target = 25
  case binarySearch sortedList target of
    Just index -> putStrLn $ "Found " ++ show target ++ " at index " ++ show index
    Nothing -> putStrLn $ "Could not find " ++ show target

  putStrLn "\nFibonacci numbers (first 10):"
  let fibNumbers = map fibonacci [0 .. 9]
  print fibNumbers

-- Main function
main :: IO ()
main = do
  putStrLn "Running Algorithm Tests\n"
  testAlgorithms
