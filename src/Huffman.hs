module Huffman where

import Data.List

type Frequency = Int
type Symbol    = Char
data Huffman   = Leaf {frequency :: Frequency, symbol :: Symbol} | Fork {frequency :: Frequency, left :: Huffman, right :: Huffman} deriving Show

instance Eq Huffman where
    (==) x y = frequency x == frequency y

instance Ord Huffman where
    (<=) x y = frequency x <= frequency y

frequencies :: String -> [Huffman]
frequencies s = [Leaf (length x) (head x) | x <- group $ sort s]

huffman :: [Huffman] -> Huffman
huffman x = build $ sort x where
    build :: [Huffman] -> Huffman
    build []      = error "Cannot build tree from an empty list."
    build [x]     = x
    build (x:y:r) = build $ insert (merge x y) r where
        merge :: Huffman -> Huffman -> Huffman
        merge x y = Fork (frequency x + frequency y) x y

codebook :: Huffman -> [(Symbol, String)]
codebook (Leaf _ s)   = [(s, "")]
codebook (Fork _ l r) = map (('0' :) <$>) (codebook l) ++ map (('1' :) <$>) (codebook r)

encode :: String -> Huffman -> String
encode string huffman = maybe "" id $ concat <$> (sequence $ map (`lookup` (codebook huffman)) string)

decode :: String -> Huffman -> String
decode string huffman = maybe "" id $ sequence $ go string huffman where
    go :: String -> Huffman -> [Maybe Char]
    go []     _       = []
    go string huffman = symbol : go leftover huffman where (symbol, leftover) = search string huffman

search :: String -> Huffman -> (Maybe Char, String)
search string (Leaf _ s)   = (Just s, string)
search []     (Fork _ _ _) = (Nothing, "")
search (h:t)  (Fork _ l r) = if h == '0' then search t l else search t r
