module Compressor where

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import Data.Bits
import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.Word
import Huffman

type Register = (Word8, Word32)

start :: (Int, Int) -> Put.Put
start (n, t) = do
    Put.putWord8 $ toEnum n
    Put.putWord32be $ toEnum t
    Put.flush

frequencies :: [(Symbol, Frequency)] -> Put.Put
frequencies []            = Put.flush
frequencies ((c, f) : xs) = do
    Put.putWord8 $ Internal.c2w c
    Put.putWord32be $ toEnum f
    Compressor.frequencies xs

word :: String -> Word8
word string = go string 7 where    
    go []     _ = 0
    go (x:xs) n = shift (read [x]) n + go xs (n - 1)

full :: String -> Put.Put
full []     = Put.flush
full string = do
    Put.putWord8 (word $ take 8 string)
    full $ drop 8 string

string :: Word8 -> String
string word = go word 128 where
    go :: Word8 -> Word8 -> String
    go _ 0 = []
    go w n | w .&. n == 0 = '0' : go w (shift n $ -1)
           | otherwise    = '1' : go w (shift n $ -1)

register :: Get.Get Register
register = do
    c <- Get.getWord8
    f <- Get.getWord32be
    return (c, f)

registers :: Get.Get [Register]
registers = do
    empty <- Get.isEmpty
    if empty
        then return []
        else do r <- register; rs <- registers; return (r : rs)

message :: Get.Get [Word8]
message = do
    empty <- Get.isEmpty
    if empty
        then return []
        else do x <- go; xs <- message; return (x : xs) where
            go :: Get.Get Word8
            go = Get.getWord8 >>= \x -> return x

leaves :: [Register] -> [Huffman]
leaves r = sort $ go r where
    go [] = []
    go ((c, f) : rs) = (Leaf (read $ show f) $ Internal.w2c c) : go rs

compress :: String -> String -> IO ()
compress origin target = do
    file <- readFile origin
    let f = Huffman.frequencies file
    let l = Huffman.leaves file
    let h = huffman l
    let encoded = encode file h
    let characters = length f
    let filling = 8 - rem (length encoded) 8 
    let a = Put.runPut $ start (characters, filling)
    let b = Put.runPut $ Compressor.frequencies f
    let c = Put.runPut $ full encoded
    Lazy.writeFile target $ a <> b <> c 

decompress :: String -> String -> IO ()
decompress origin target = do
    bytestring <- Lazy.readFile origin
    let h@(n, t) = Get.runGet register bytestring
    let r = Get.runGet registers $  Lazy.take ((read $ show n) * 5) $ Lazy.drop 5 bytestring
    let m = Get.runGet message $ Lazy.drop ((read $ show n) * 5 + 5) bytestring
    let f = Compressor.leaves r
    let a = concat $ map string m
    let b = take (length a - fromEnum t) a
    let decoded = decode b $ huffman f
    writeFile target decoded
