module TreeConstructors where

import Data.Tree

strField :: String -> String -> Tree String
strField name value = Node (name) [Node (value) []]

numField :: String -> Double -> Tree String
numField name value = Node (name) [Node (show value) []]

node :: String -> [Tree String] -> Tree String
node name fields = Node name fields