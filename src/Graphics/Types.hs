module Types where

import qualified Data.ByteString     as B
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Linear

data STL = STL { header :: B.ByteString, numFacets :: Word32, triangles :: V.Vector Triangle }
  deriving (Show, Eq)

type Header = B.ByteString

type Triangle = V4 (V3 Float)
