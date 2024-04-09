{-# LANGUAGE UnicodeSyntax #-}

module Huffman where

import Data.List
import Synonyms

data Huffman = Leaf Frequency Symbol | Fork Frequency Huffman Huffman deriving Show

instance Eq Huffman where
    (==) x y = frequency x == frequency y

instance Ord Huffman where
    (<=) x y = frequency x <= frequency y

frequencies ∷ String → [Huffman]
frequencies s = [Leaf (length x) (head x) | x ← group $ sort s]

frequency ∷ Huffman → Frequency
frequency (Leaf f _)   = f
frequency (Fork f _ _) = f

merge ∷ Huffman → Huffman → Huffman
merge x y = Fork (frequency x + frequency y) x y

huffman ∷ [Huffman] → Huffman
huffman x = build $ sort x where
    build ∷ [Huffman] → Huffman
    build []      = error "Error"
    build [x]     = x
    build (x:y:r) = build $ insert (merge x y) r

dictionary ∷ Huffman → [(Symbol, String)]
dictionary (Leaf _ s)   = [(s, "")]
dictionary (Fork _ l r) = map (('0' :) <$>) (dictionary l) ++ map (('1' :) <$>) (dictionary r)

encode ∷ String → Huffman → String
encode string huffman = maybe "" id $ concat <$> (sequence $ map (`lookup` (dictionary huffman)) string)

decode ∷ String → Huffman → Maybe String
decode string huffman = undefined

search ∷ String → Huffman → (Maybe Char, String)
search string (Leaf _ s)   = (Just s, string) 
search []     (Fork _ _ _) = (Nothing, "")
search (h:t)  (Fork _ l r) = if h == '0' then search t l else search t r

h = huffman $ frequencies "cavalo"
