module Main where

import Control.Monad
import Happstack.Server (nullConf, simpleHTTP, ok, dir, seeOther)

main :: IO ()
main = simpleHTTP nullConf $ msum 
    [ dir "hello" $ ok "Hello, World\n",
      dir "goodbye" $ ok "Goodbye, World\n",
      seeOther "/hello" "/hello"]
