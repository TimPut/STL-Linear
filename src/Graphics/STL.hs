-- |
-- Module      : Graphics.STL
-- Copyright   : tim put <timput@gmail.com> 2021
-- License     : MIT
--
-- Maintainer  : timput@gmail.com
--
-- Provides types for representing STL 3D models and fast
-- serialization and deserialization for both binary and ASCII STL
-- standards
--
-- The representation is backed by unboxed vectors. Some applications
-- may benefit from the lazy list backed version found in
-- "Graphics.STL.Lazy"


{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.STL ( STL(..)
                    , Header
                    , Triangle
                    , parseSTL
                    , unparseSTL
                    ) where

import           Graphics.STL.Common
import           Control.Monad                    ((>=>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary                      (Binary, get, put)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as B
import qualified Data.Vector.Unboxed              as V
import           Data.Word
import           Linear
import           Control.DeepSeq
import           GHC.Generics                     (Generic)

data STL = STL
    { header    :: B.ByteString
    , numTriangles :: Word32
    , normals :: V.Vector Normal
    , triangles :: V.Vector Triangle
    }
  deriving (NFData, Generic, Eq, Show)

getSTL :: Get STL
getSTL = do
  header' <- getHeaderB
  numFacets' <- getWord32le
  (normals', triangles') <- V.unzip <$> V.replicateM (fromIntegral numFacets') getTriangleB
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
                      traverse (putTriangle >=> \_ -> (putWord16le 0)) (V.toList $ V.zip ns ts)
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
  (normals', triangles') <- V.unzip . V.fromList <$> many1' parseFacet
  string "endsolid" *> many1' anyChar  *> endOfInput
  return (STL name (fromIntegral $ V.length triangles') normals' triangles')

unparseSTL :: STL -> B.ByteString
unparseSTL (STL name _numFacets' normals' triangles') = let shortName = B.takeWhile (/= 0) name in
    "solid " <> shortName <> "\n" <>
    B.concat (fmap unparseFacet (V.toList $ V.zip normals' triangles')) <>
    "endsolid " <> shortName
