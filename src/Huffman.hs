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
    build []      = error "Error"
    build [x]     = x
    build (x:y:r) = build $ insert (merge x y) r

dictionary ∷ Huffman → [(Symbol, String)]
dictionary (Leaf _ s)   = [(s, "")]
dictionary (Fork _ l r) = map (('0' :) <$>) (dictionary l) ++ map (('1' :) <$>) (dictionary r)

encode ∷ String → Huffman → Maybe String
encode string huffman = concat <$> (sequence $ map (`lookup` (dictionary huffman)) string)

decode ∷ String → Huffman → Maybe String
decode []     _       = Just ""
decode string huffman = fst (search string huffman) : decode (snd $ search string huffman) huffman where
    search c (Leaf _ s)   = if c == s then Just
    search c (Fork _ l r) = max ('0' : (search c l)) ('1' : (search c r)) 


h = huffman $ frequencies "cavalo"