module Lib (someFunc) where

import System.Random

type Weights = [Int]
type Inputs = [Int]
type Tau = Int

data Network = Network { neuronsPerLayer :: Int
                       , inputsPerNeuron :: Int
                       , weightRange :: Int
                       , weights :: [Weights]
                       } deriving (Eq)

instance Show Network where
    show n = show (weights n)

sgn :: Int -> Int
sgn x | x < 0 = -1
      | x == 0 = 0
      | x > 0 = 1

sigma :: Weights -> Inputs -> Int
sigma ws xs = sgn . foldr (+) 0 $ zipWith (*) ws xs

tau :: [Weights] -> [Inputs] -> Int
tau inputs nodes = foldr (*) 1 $ zipWith sigma inputs nodes

theta :: Int -> Int -> Int
theta x y | x == y = 1
          | otherwise = 0

g r x | x > r = r
      | x < (-1 * r) = -r
      | otherwise = x

hebbian :: Int -> Tau -> Tau -> Weights -> Inputs -> Weights
hebbian r t1 t2 ws xs = f $ zip ws xs
    where s = sigma ws xs
          f = fmap (\(w, x) -> g r (w + (s * x * (theta s t1))))

updateNetwork :: Network -> Tau -> Tau -> [Inputs] -> Network
updateNetwork (Network n i r ws) t1 t2 inputs = Network n i r ws'
    where ws' = zipWith (hebbian r t1 t2) ws inputs

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

createNetwork :: StdGen -> Int -> Int -> Int -> Network
createNetwork g k n l = do
    let ws = take (k*n) $ randomRs (-1 * l, l) g
        in Network k n l (group n ws)

randomInput :: Int -> Int -> IO [Inputs]
randomInput k n = group n . take (k*n) . fmap binary . randoms <$> newStdGen
    where binary True = 1
          binary False = -1

sync_step :: [Inputs] -> (Network, Network) ->  (Network, Network)
sync_step inputs (n1@(Network k n _ _), n2)  = (n1', n2')
    where (t1,t2) = (tau (weights n1) inputs , tau (weights n2) inputs)
          (n1',n2') = if t1 == t2
                      then (updateNetwork n1 t1 t2 inputs, updateNetwork n2 t1 t2 inputs)
                      else (n1, n2)

simulation = do
    let (k, n, l) = (8, 8, 5)
    let epochs = 10000
    a <- newStdGen
    b <- newStdGen
    let n1 = createNetwork a k n l 
    let n2 = createNetwork b k n l
    print $ n1 == n2
    putStrLn $ "Syncing networks..."
    inputs <- sequence . take epochs $ repeat (randomInput k n)
    let (n1',n2') = (foldr1 (.) $ fmap sync_step inputs) (n1, n2)
    print $ n1' == n2'


someFunc = print "Hello"
