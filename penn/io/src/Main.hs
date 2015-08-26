{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Bits (xor)

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret first second = do
    firstContent <- BS.readFile first
    secondContent <- BS.readFile second
    let decoded = decode firstContent secondContent
    return decoded

decode :: ByteString -> ByteString -> ByteString
decode first second = filter . map xor  

main :: IO ()
main = do
  putStrLn "hello world"
