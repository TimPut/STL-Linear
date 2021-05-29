-- STL.hs --- 

-- Copyright (C) 2021 tim put <timput@gmail.com>

-- Author: tim put <timput@gmail.com>

-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- Except as contained in this notice, the name(s) of the above copyright
-- holders shall not be used in advertising or otherwise to promote the sale,
-- use or other dealings in this Software without prior written authorization.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

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

-- |
-- The ASCII STL standard allows a single arbitrarily long line as a
-- header, while the binary STL standard uses a fixed 80 byte header.
-- This means that conversions between the two are not exact.
--
-- When serializing an STL to binary, we retain the first 80 bytes of
-- the STL's header, zero padded if necessary, while when serializing
-- an STL to ASCII we drop any trailing zero bytes.
--
-- Note that when parsing a binary STL, the full 80 byte header is
-- read in without triming any trailing zero bytes.
type Header = B.ByteString

-- |
-- Triangles are composed of a normal vector represented as V3 Float,
-- and three vertices represented similarly.
--
-- Note that the normal is stored as the first component of the V4,
-- while the remaining three components store the vertices.
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

-- |
-- >>> import Data.Binary (Binary, encode, decode)
-- >>> import Data.ByteString.Lazy (readFile, writeFile)
--
-- >>> stl <- decode $ readFile "./example.stl" :: IO STL
-- >>> writeFile "./exampleOut.stl" (encode stl)
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
