module RandomNumber where

  import System.Random

  {- getRandomN
    seed = seed value

    returns a single non negative random Int
  -}
  getRandomN :: Int -> Int
  getRandomN seed = head (getRandomLs 1 seed)

  {- getRandomls
    n    = number of items in list
    seed = seed value

    returns a list of random non negative Ints
  -}
  getRandomLs :: Int -> Int -> [Int]
  getRandomLs n seed = randomNumbersS
                       where
                         randomNumbersU = (take n (randoms (mkStdGen seed) :: [Int]))
                         randomNumbersS = sortNegativeLs randomNumbersU

  {- sortNegatives
    returns a list of postive Ints
  -}
  sortNegativeLs :: [Int] -> [Int]
  sortNegativeLs [] = []
  sortNegativeLs (x:xs)
   | x<0 = (x * (-1)) : sortNegativeLs xs
   | otherwise = x : sortNegativeLs xs
