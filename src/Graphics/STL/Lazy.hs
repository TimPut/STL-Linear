{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.STL.Lazy ( STL(..)
                         , Header
                         , Triangle
                         , parseSTL
                         , unparseSTL
                         ) where

import           Graphics.STL.Common
import           Control.Monad                    ((>=>),replicateM)
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary                      (Binary, get, put)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as B
import           Data.Word
import           Linear
import           Control.DeepSeq
import           GHC.Generics                     (Generic)

data STL = STL
    { header    :: B.ByteString
    , numTriangles :: Word32
    , normals :: [Normal]
    , triangles :: [Triangle]
    }
  deriving (NFData, Generic, Eq, Show)

getSTL :: Get STL
getSTL = do
  header' <- getHeaderB
  numFacets' <- getWord32le
  (normals', triangles') <- unzip <$> replicateM (fromIntegral numFacets') getTriangleB
  return $! STL header' numFacets' normals' triangles'
{-# INLINE getSTL #-}

-- |
-- >>> import Data.Binary (Binary, encode, decode)
-- >>> import Data.ByteString.Lazy (readFile, writeFile)
--
-- >>> stl <- decode $ readFile "./example.stl" :: IO STL
-- >>> writeFile "./exampleOut.stl" (encode stl)
instance Binary STL where
    put (STL h n ns ts) = do
                      let h' = B.take 80 $ h <> B.replicate 80 32
                      putByteString h'
                      putWord32le n
                      traverse (putTriangle >=> \_ -> (putWord16le 0)) (zip ns ts)
                      return ()
      where
        -- TODO: dry this out
        putTriangle ((V3 a b c), V3 (V3 a' b' c') (V3 a'' b'' c'') (V3 a''' b''' c''')) = do
                         sequenceA $ fmap putFloatle [ a
                                                     , b
                                                     , c
                                                     , a'
                                                     , b'
                                                     , c'
                                                     , a''
                                                     , b''
                                                     , c''
                                                     , a'''
                                                     , b'''
                                                     , c'''
                                                     ]
        {-# INLINE putTriangle #-}
    get = getSTL

parseSTL :: Parser STL
parseSTL = do
  name <- parseHeader
  (normals', triangles') <- unzip <$> many1' parseFacet
  string "endsolid" *> many1' anyChar  *> endOfInput
  return (STL name (fromIntegral $ length triangles') normals' triangles')

unparseSTL :: STL -> B.ByteString
unparseSTL (STL name _numFacets' normals' triangles') = let shortName = B.takeWhile (/= 0) name in
    "solid " <> shortName <> "\n" <>
    B.concat (fmap unparseFacet (zip normals' triangles')) <>
    "endsolid " <> shortName
