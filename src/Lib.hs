{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import System.Random
import NeuralNet
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as B
import Crypto.Cipher.AES

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
    print "alice:" >> mapM_ print (weights n1)
    print "bob:" >> mapM_ print (weights  n2)
    putStrLn $ "Syncing networks..."
    inputs <- sequence . take epochs $ repeat (randomInput k n)
    let (n1',n2') = (foldr1 (.) $ fmap sync_step inputs) (n1, n2)
    print "alice:" >> mapM_ print (weights n1')
    print "bob:" >> mapM_ print (weights n2')
    let key = encode (concat . weights $ n1')
    print "Key is: " >> B.putStrLn key
    let aes = initAES $ B.toStrict (B.take 32 key)
    let encrypted = (encryptECB aes "This is a test  ")
    B.putStrLn $ B.fromStrict encrypted
    let message' = decryptECB aes encrypted
    B.putStrLn $ B.fromStrict message'
    
someFunc = print "Hello"
