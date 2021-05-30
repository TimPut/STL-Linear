module Main (main) where

import           Control.Monad
import           Data.Attoparsec.ByteString hiding (try)
import           Data.Binary
import qualified Data.ByteString.Lazy as L
import           Graphics.STL
import qualified Graphics.STL.Lazy as L
import           System.Exit (exitFailure)

roundtripBinary' :: L.ByteString -> L.ByteString
roundtripBinary' = encode . (decode :: L.ByteString -> STL)

roundtripAscii' :: L.ByteString -> L.ByteString
roundtripAscii' bs = case parseOnly parseSTL $ L.toStrict bs of
                       Left e -> error e
                       Right r -> L.fromStrict $ unparseSTL r

roundtripBinary :: L.ByteString -> L.ByteString
roundtripBinary = encode . (decode :: L.ByteString -> L.STL)

roundtripAscii :: L.ByteString -> L.ByteString
roundtripAscii bs = case parseOnly L.parseSTL $ L.toStrict bs of
                      Left e -> error e
                      Right r -> L.fromStrict $ L.unparseSTL r

main :: IO ()
main = do  
  binaryStlData <- L.readFile "./models/Binary.stl"
  asciiStlData <- L.readFile "./models/ASCII.stl"

  when (
    binaryStlData /= roundtripBinary binaryStlData ||
    binaryStlData /= roundtripBinary' binaryStlData)
    (putStrLn "Binary roundtrip failed" >> exitFailure)

  -- The ascii round trip normalizes negative zeroes to positive
  -- zeroes, but this should be idempotent (once normalized, always
  -- normalized) so we compare single round trip to double round trip
  when (
     roundtripAscii asciiStlData /= roundtripAscii (roundtripAscii asciiStlData) ||
     roundtripAscii' asciiStlData /= roundtripAscii' (roundtripAscii' asciiStlData))
     (putStrLn "ASCII roundtrip failed" >> exitFailure)

     
