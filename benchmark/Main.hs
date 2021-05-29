{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

import           Data.Attoparsec.ByteString hiding (try)
-- import qualified Data.Attoparsec.Text       as T
import           Data.Binary
import qualified Data.ByteString            as B
import           Data.ByteString.Lazy       (fromStrict)
-- import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Gauge
--import qualified Graphics.Formats.STL       as F
import           Graphics.STL

-- import           Control.DeepSeq
-- import           GHC.Generics               (Generic)
import           Control.Exception

main :: IO ()
main = do
  file <- try (B.readFile "./benchmark/3DBenchy.stl") :: IO (Either SomeException B.ByteString)
  case file of
    Left ex -> putStrLn "Could not find ./benchmark/3DBenchy.stl, aborting benchmarking"
    Right stl -> do
      let r = decode (fromStrict stl) :: STL
      B.writeFile "./benchmark/ASCII3DBenchy.stl" (unparseSTL r)
      defaultMainWith (defaultConfig {quickMode = True})
        [ env setupEnv $
          \ ~(ascii,binary,text) -> bgroup "main"
                                   [ bgroup "ascii" [
                                       bench "STL-Linear parse"
                                       (nf asciiStlLinear ascii)
                                       -- , bench "STL parse"
                                       -- (nf asciiStl text)
                                       ]
                                   , bgroup "binary" [
                                       bench "STL-Linear parse"
                                       (nf binaryStl binary)
                                       ]
                                   ]
        ]

setupEnv = do
  asciiSTL <- B.readFile "./benchmark/ASCII3DBenchy.stl"
  binarySTL <- B.readFile "./benchmark/3DBenchy.stl"
  textSTL <- T.readFile "./benchmark/ASCII3DBenchy.stl"
  return (asciiSTL, binarySTL, textSTL)

binaryStl :: B.ByteString -> STL
binaryStl bs = decode (fromStrict bs)

asciiStlLinear ::  B.ByteString -> STL
asciiStlLinear bs = let Right r = parseOnly parseSTL bs in r

--asciiStl :: T.Text -> F.STL
--asciiStl ts = let Right r = T.parseOnly F.stlParser ts in r

-- deriving instance Generic (F.Triangle)
-- deriving instance NFData (F.Triangle)
-- deriving instance Generic (F.STL)
-- deriving instance NFData (F.STL)
