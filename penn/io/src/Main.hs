{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Bits (xor)
import Data.List (find)
import Parser

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
    encContent <- BS.readFile $ path ++ ".enc"
    let decrypted = pairXOR (BS.cycle key) encContent
    BS.writeFile path decrypted

pairXOR :: ByteString -> ByteString -> ByteString
pairXOR first second = BS.pack . 
    map (\(a, b) -> a `xor` b) $ 
    BS.zip first second

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret first second = do
    firstContent <- BS.readFile first
    secondContent <- BS.readFile second
    let decrypted = decrypt firstContent secondContent
    return decrypted

decrypt :: ByteString -> ByteString -> ByteString
decrypt first second =
    BS.filter (\val -> val /= 0) $ 
    pairXOR first second 

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
    content <- BS.readFile path
    return $ decode content

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimPath allPath = do
    victims <- parseFile victimPath :: IO (Maybe [TId])
    transactions <- parseFile allPath :: IO (Maybe [Transaction])
    let filtering transaction = case find (\v -> tid transaction == v) <$> victims of
            Nothing -> False
            Just _ -> True
    let helper transactions _ = filter filtering <$> transactions
    return $ helper transactions victims

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = foldl helper Map.empty transactions 
    where
    helper :: Map String Integer -> Transaction -> Map String Integer
    helper prev elem = let
        _to = to elem
        _from = from elem
        _amount = amount elem
        in Map.insertWith (+) _from (-_amount) $ 
            Map.insertWith (+) _to _amount prev

getCriminal :: Map String Integer -> String
getCriminal = 
    fst . 
    Map.foldlWithKey (\ks@(_,prev) k v -> 
        if prev >= v 
            then ks 
            else (k,v)) ("", 0)

main :: IO ()
main = do
  putStrLn "hello world"
