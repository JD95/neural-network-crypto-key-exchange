module NeuralNet ( Network(..)
                 , Weights
                 , Inputs
                 , Tau
                 , tau
                 , updateNetwork
                 , createNetwork
                 , randomInput
                 ) where

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
-- | The learning rule
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
