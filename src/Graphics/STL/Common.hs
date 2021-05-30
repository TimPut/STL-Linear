{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.STL.Common ( Header
                           , Triangle
                           , Normal
                           , getHeaderB
                           , getVertexB
                           , getTriangleB
                           , parseHeader
                           , parseFacet
                           , parseV3
                           , unparseFacet
                           , unparseV3
                           ) where

import           Data.Attoparsec.ByteString.Char8
import           Data.Binary.Get
import qualified Data.ByteString                  as B hiding (pack)
import           Data.ByteString.Char8            (pack)
import           Data.Scientific                  (toRealFloat)
import           Linear

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
--
-- Binary STLs also support a two-byte attribute field on each facet.
-- This is not supported in ASCII STLs and its use is not
-- standardized. We discard this data when deserializing and set the
-- attribute field to two zero bytes when serializing.
type Header = B.ByteString

type Triangle = V3 (V3 Float)
type Normal = V3 Float

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

getTriangleB :: Get (Normal, Triangle)
getTriangleB = do
  n <- getVertexB
  a <- getVertexB
  b <- getVertexB
  c <- getVertexB
  _ <- getWord16le -- two-byte attribute, typically unused
  return $! (n, V3 a b c)
{-# INLINE getTriangleB #-}

parseHeader :: Parser B.ByteString
parseHeader = do
  string "solid"
  skipSpace
  name <- many' (notChar '\n')
  endOfLine
  return (pack name)

parseFacet :: Parser (Normal, Triangle)
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
  return (n, V3 a b c)

parseV3 :: Parser (V3 Float)
parseV3 = do
  a <- toRealFloat <$> scientific
  skipSpace
  b <- toRealFloat <$> scientific
  skipSpace
  c <- toRealFloat <$> scientific
  return (V3 a b c)

unparseFacet :: (Normal, Triangle) -> B.ByteString
unparseFacet (n, V3 a b c) =
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
