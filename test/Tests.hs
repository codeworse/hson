{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Binary.Put (runPut, putDoublele)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word32, Word64)
import Data.Bits (shiftR, (.&.))

import Hson

-- ---------------------------------------------------------------------------
-- Encoding helpers

le32 :: Int32 -> [Word8]
le32 n =
    let w = fromIntegral n :: Word32
    in map (\s -> fromIntegral ((w `shiftR` s) .&. 0xFF)) [0, 8, 16, 24]

le64 :: Int64 -> [Word8]
le64 n =
    let w = fromIntegral n :: Word64
    in map (\s -> fromIntegral ((w `shiftR` s) .&. 0xFF)) [0, 8, 16, 24, 32, 40, 48, 56]

leDouble :: Double -> [Word8]
leDouble d = BL.unpack (runPut (putDoublele d))

-- cstring: UTF-8 bytes + null terminator
cstr :: String -> [Word8]
cstr s = map (fromIntegral . fromEnum) s ++ [0x00]

-- BSON string value: int32(len+1) + utf8 + null
bsonStr :: String -> [Word8]
bsonStr s = le32 (fromIntegral (length s + 1)) ++ map (fromIntegral . fromEnum) s ++ [0x00]

-- Build a single BSON element: type + cstring key + raw value bytes
elem' :: Word8 -> String -> [Word8] -> [Word8]
elem' t key val = [t] ++ cstr key ++ val

-- Wrap element byte lists in a BSON document envelope
mkDoc :: [[Word8]] -> BS.ByteString
mkDoc elems = BS.pack (le32 size ++ concat elems ++ [0x00])
  where
    size = fromIntegral (4 + sum (map length elems) + 1) :: Int32

-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Hson"
  [ testGroup "Scalar types"
      [ testCase "Int32" $ do
          let doc = mkDoc [elem' 0x10 "n" (le32 42)]
          parseBson doc @?= Right [("n", BsonInt32 42)]

      , testCase "Int32 negative" $ do
          let doc = mkDoc [elem' 0x10 "n" (le32 (-1))]
          parseBson doc @?= Right [("n", BsonInt32 (-1))]

      , testCase "Int64" $ do
          let doc = mkDoc [elem' 0x12 "n" (le64 1000000000000)]
          parseBson doc @?= Right [("n", BsonInt64 1000000000000)]

      , testCase "Double" $ do
          let doc = mkDoc [elem' 0x01 "d" (leDouble 3.14)]
          parseBson doc @?= Right [("d", BsonDouble 3.14)]

      , testCase "String" $ do
          let doc = mkDoc [elem' 0x02 "s" (bsonStr "hello")]
          parseBson doc @?= Right [("s", BsonString (T.pack "hello"))]

      , testCase "String empty" $ do
          let doc = mkDoc [elem' 0x02 "s" (bsonStr "")]
          parseBson doc @?= Right [("s", BsonString (T.pack ""))]

      , testCase "Bool true" $ do
          let doc = mkDoc [elem' 0x08 "b" [0x01]]
          parseBson doc @?= Right [("b", BsonBool True)]

      , testCase "Bool false" $ do
          let doc = mkDoc [elem' 0x08 "b" [0x00]]
          parseBson doc @?= Right [("b", BsonBool False)]

      , testCase "Null" $ do
          let doc = mkDoc [elem' 0x0A "x" []]
          parseBson doc @?= Right [("x", BsonNull)]

      , testCase "UTCTime zero" $ do
          let doc = mkDoc [elem' 0x09 "t" (le64 0)]
          parseBson doc @?= Right [("t", BsonUtcTime 0)]

      , testCase "UTCTime non-zero" $ do
          let doc = mkDoc [elem' 0x09 "t" (le64 1700000000000)]
          parseBson doc @?= Right [("t", BsonUtcTime 1700000000000)]

      , testCase "Binary" $ do
          let payload = [0x01, 0x02, 0x03]
          let doc = mkDoc [elem' 0x05 "d" (le32 3 ++ [0x00] ++ payload)]
          parseBson doc @?= Right [("d", BsonBinary 0x00 (BS.pack payload))]

      , testCase "Binary with custom subtype" $ do
          let payload = [0xAB, 0xCD]
          let doc = mkDoc [elem' 0x05 "d" (le32 2 ++ [0x80] ++ payload)]
          parseBson doc @?= Right [("d", BsonBinary 0x80 (BS.pack payload))]
      ]

  , testGroup "Document structure"
      [ testCase "empty document" $ do
          parseBson (BS.pack (le32 5 ++ [0x00])) @?= Right []

      , testCase "multiple fields" $ do
          let doc = mkDoc [ elem' 0x10 "a" (le32 1)
                          , elem' 0x10 "b" (le32 2)
                          , elem' 0x10 "c" (le32 3)
                          ]
          parseBson doc @?= Right
            [ ("a", BsonInt32 1)
            , ("b", BsonInt32 2)
            , ("c", BsonInt32 3)
            ]

      , testCase "mixed-type fields" $ do
          let doc = mkDoc [ elem' 0x10 "i"   (le32 7)
                          , elem' 0x02 "s"   (bsonStr "hi")
                          , elem' 0x08 "ok"  [0x01]
                          , elem' 0x0A "nil" []
                          ]
          parseBson doc @?= Right
            [ ("i",   BsonInt32 7)
            , ("s",   BsonString (T.pack "hi"))
            , ("ok",  BsonBool True)
            , ("nil", BsonNull)
            ]

      , testCase "embedded document" $ do
          let innerBytes = BS.unpack (mkDoc [elem' 0x10 "x" (le32 99)])
          let doc = mkDoc [elem' 0x03 "inner" innerBytes]
          parseBson doc @?= Right [("inner", BsonDoc (T.pack "x", BsonInt32 99))]

      , testCase "array of ints" $ do
          let arrBytes = BS.unpack $ mkDoc
                [ elem' 0x10 "0" (le32 10)
                , elem' 0x10 "1" (le32 20)
                , elem' 0x10 "2" (le32 30)
                ]
          let doc = mkDoc [elem' 0x04 "arr" arrBytes]
          parseBson doc @?= Right
            [("arr", BsonArray [BsonInt32 10, BsonInt32 20, BsonInt32 30])]

      , testCase "array of strings" $ do
          let arrBytes = BS.unpack $ mkDoc
                [ elem' 0x02 "0" (bsonStr "a")
                , elem' 0x02 "1" (bsonStr "b")
                ]
          let doc = mkDoc [elem' 0x04 "strs" arrBytes]
          parseBson doc @?= Right
            [("strs", BsonArray [BsonString (T.pack "a"), BsonString (T.pack "b")])]

      , testCase "empty array" $ do
          let arrBytes = BS.unpack (BS.pack (le32 5 ++ [0x00]))
          let doc = mkDoc [elem' 0x04 "arr" arrBytes]
          parseBson doc @?= Right [("arr", BsonArray [])]
      ]

  , testGroup "Errors"
      [ testCase "unknown type byte" $ do
          let doc = mkDoc [elem' 0xFF "x" []]
          case parseBson doc of
            Left  _ -> return ()
            Right _ -> assertFailure "expected parse failure for unknown type 0xFF"

      , testCase "truncated input" $ do
          case parseBson (BS.pack [0x10, 0x00, 0x00]) of
            Left  _ -> return ()
            Right _ -> assertFailure "expected parse failure for truncated input"

      , testCase "wrong terminator" $ do
          -- well-sized document but terminator byte is 0x01 instead of 0x00
          case parseBson (BS.pack (le32 5 ++ [0x01])) of
            Left  _ -> return ()
            Right _ -> assertFailure "expected parse failure for wrong terminator"

      , testCase "empty input" $ do
          case parseBson BS.empty of
            Left  _ -> return ()
            Right _ -> assertFailure "expected parse failure for empty input"
      ]
  ]
