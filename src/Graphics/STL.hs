-- STL.hs ---

-- Copyright (C) 2020 tim put <tim@timput.com>

-- Author: tim put <tim@timput.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-
Binary and ASCII de/serialization for STL files
-}

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

import           Control.Monad                    ((>=>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary                      (Binary, get, put)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as B hiding (pack)
import           Data.ByteString.Char8            (pack)
import           Data.Scientific                  (toRealFloat)
import qualified Data.Vector.Unboxed              as V
import           Data.Word
import           Linear

import           Control.DeepSeq
import           GHC.Generics                     (Generic)

data STL = STL
    { header    :: B.ByteString
    , numFacets :: Word32
    , triangles :: V.Vector Triangle
    }
    deriving (Show, Eq, Generic, NFData)

type Header = B.ByteString

type Triangle = V4 (V3 Float)

getHeaderB :: Get Header
getHeaderB = do
  header' <- getByteString 80
  return $! header'
{-# INLINE getHeaderB #-}

getVertexB :: Get (V3 Float)
getVertexB = do
   x <- getFloatle
   y <- getFloatle
   z <- getFloatle
   return $! (V3 x y z)
{-# INLINE getVertexB #-}

getTriangleB :: Get Triangle
getTriangleB = do
  n <- getVertexB
  a <- getVertexB
  b <- getVertexB
  c <- getVertexB
  _ <- getWord16le -- two-byte attribute
  return $! V4 n a b c
{-# INLINE getTriangleB #-}

getSTL :: Get STL
getSTL = do
  header' <- getHeaderB
  numFacets' <- getWord32le
  triangles' <- V.replicateM (fromIntegral numFacets') getTriangleB
  return $! STL header' numFacets' triangles'
{-# INLINE getSTL #-}

instance Binary STL where
    put (STL h n ts) = do
                      let h' = B.take 80 $ h <> B.replicate 80 32
                      putByteString h'
                      putWord32le n
                      traverse (putTriangle >=> \_ -> (putWord16le 0)) (V.toList ts)
                      return ()
      where
        -- TODO: dry this out
        putTriangle (V4 (V3 a b c) (V3 a' b' c') (V3 a'' b'' c'') (V3 a''' b''' c''')) = do
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

parseHeader :: Parser B.ByteString
parseHeader = do
  string "solid"
  skipSpace
  name <- many' (notChar '\n')
  endOfLine
  return (pack name)

parseFacet :: Parser Triangle
parseFacet = do
  skipSpace
  string "facet normal"
  skipSpace
  n <- parseV3
  endOfLine
  skipSpace
  string "outer loop"
  endOfLine
  let parseVertex = do
               skipSpace
               string "vertex"
               skipSpace
               v <- parseV3
               endOfLine
               return v
  a <- parseVertex
  b <- parseVertex
  c <- parseVertex
  skipSpace *> string "endloop" *> endOfLine
  skipSpace *> string "endfacet" *> endOfLine
  return (V4 n a b c)

parseV3 :: Parser (V3 Float)
parseV3 = do
  a <- toRealFloat <$> scientific
  skipSpace
  b <- toRealFloat <$> scientific
  skipSpace
  c <- toRealFloat <$> scientific
  return (V3 a b c)

parseSTL :: Parser STL
parseSTL = do
  name <- parseHeader
  triangles' <- V.fromList <$> many1' parseFacet
  string "endsolid" *> many1' anyChar  *> endOfInput
  return (STL name (fromIntegral $ V.length triangles') triangles')

unparseSTL :: STL -> B.ByteString
unparseSTL (STL name _numFacets' triangles') = let shortName = B.takeWhile (/= 0) name in
    "solid " <> shortName <> "\n" <>
    B.concat (fmap unparseFacet (V.toList triangles')) <>
    "endsolid " <> shortName

unparseFacet :: Triangle -> B.ByteString
unparseFacet (V4 n a b c) =
    "  facet normal " <> unparseV3 n <> "\n" <>
    "    outer loop\n" <>
    "      vertex " <> unparseV3 a <> "\n" <>
    "      vertex " <> unparseV3 b <> "\n" <>
    "      vertex " <> unparseV3 c <> "\n" <>
    "    endloop\n" <>
    "  endfacet\n"

unparseV3 :: V3 Float -> B.ByteString
unparseV3 (V3 a b c) = pack $
    show a <> " " <> show b <> " " <> show c
