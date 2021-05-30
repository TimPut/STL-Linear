module Main (main) where

import           Control.Exception
import           Data.Attoparsec.ByteString hiding (try)
import           Data.Binary
import qualified Data.ByteString            as B
import           Data.ByteString.Lazy       (fromStrict)
import           Gauge
import           Graphics.STL
import qualified Graphics.STL.Lazy as L

main :: IO ()
main = do
  file <- try (B.readFile binaryStlPath) :: IO (Either SomeException B.ByteString)
  case file of
    Left _ -> putStrLn $ "Could not find " ++ binaryStlPath ++ ", aborting benchmarking"
    Right stl -> do
      let r = decode (fromStrict stl) :: STL
      B.writeFile asciiStlPath (unparseSTL r)
      defaultMainWith (defaultConfig {quickMode = True})
        [ env setupEnv $
          \ ~(ascii,binary) -> bgroup "main"
                              [ bgroup "ascii" [
                                  bench "STL-Linear parse"
                                    (nf asciiStl' ascii)
                                  , bench "STL-Linear lazy parse"
                                    (nf asciiStl ascii)
                                  ]
                              , bgroup "binary" [
                                  bench "STL-Linear parse"
                                    (nf binaryStl' binary)
                                  , bench "STL-Linear lazy parse"
                                    (nf binaryStl binary)
                                  , bench "STL-Linear lazy header only parse"
                                    (nf (L.header . binaryStl) binary)

                                  ]
                              ]
        ]

asciiStlPath, binaryStlPath :: String
asciiStlPath = "./models/ASCII.stl"
-- Replace this model with something heavier, like the 3DBenchy for
-- meaningful benchmarks
binaryStlPath = "./models/Binary.stl"

setupEnv :: IO (B.ByteString, B.ByteString)
setupEnv = do
  asciiSTL <- B.readFile asciiStlPath
  binarySTL <- B.readFile binaryStlPath
  return (asciiSTL, binarySTL)

binaryStl' :: B.ByteString -> STL
binaryStl' bs = decode (fromStrict bs)

binaryStl :: B.ByteString -> L.STL
binaryStl bs = decode (fromStrict bs)

asciiStl' ::  B.ByteString -> STL
asciiStl' bs = case parseOnly parseSTL bs of
                 Left e -> error e
                 Right r -> r

asciiStl ::  B.ByteString -> L.STL
asciiStl bs = case parseOnly L.parseSTL bs of
                 Left e -> error e
                 Right r -> r
