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
import Control.Exception (evaluate, try, ErrorCall(..))

import qualified Data.HashMap.Strict as HM
import Hson
import Hson.Get
import Hson.Merge

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
          parseBson doc @?= Right (HM.fromList [("n", BsonInt32 42)])

      , testCase "Int32 negative" $ do
          let doc = mkDoc [elem' 0x10 "n" (le32 (-1))]
          parseBson doc @?= Right (HM.fromList [("n", BsonInt32 (-1))])

      , testCase "Int64" $ do
          let doc = mkDoc [elem' 0x12 "n" (le64 1000000000000)]
          parseBson doc @?= Right (HM.fromList [("n", BsonInt64 1000000000000)])

      , testCase "Double" $ do
          let doc = mkDoc [elem' 0x01 "d" (leDouble 3.14)]
          parseBson doc @?= Right (HM.fromList [("d", BsonDouble 3.14)])

      , testCase "String" $ do
          let doc = mkDoc [elem' 0x02 "s" (bsonStr "hello")]
          parseBson doc @?= Right (HM.fromList [("s", BsonString (T.pack "hello"))])

      , testCase "String empty" $ do
          let doc = mkDoc [elem' 0x02 "s" (bsonStr "")]
          parseBson doc @?= Right (HM.fromList [("s", BsonString (T.pack ""))])

      , testCase "Bool true" $ do
          let doc = mkDoc [elem' 0x08 "b" [0x01]]
          parseBson doc @?= Right (HM.fromList [("b", BsonBool True)])

      , testCase "Bool false" $ do
          let doc = mkDoc [elem' 0x08 "b" [0x00]]
          parseBson doc @?= Right (HM.fromList [("b", BsonBool False)])

      , testCase "Null" $ do
          let doc = mkDoc [elem' 0x0A "x" []]
          parseBson doc @?= Right (HM.fromList [("x", BsonNull)])

      , testCase "UTCTime zero" $ do
          let doc = mkDoc [elem' 0x09 "t" (le64 0)]
          parseBson doc @?= Right (HM.fromList [("t", BsonUtcTime 0)])

      , testCase "UTCTime non-zero" $ do
          let doc = mkDoc [elem' 0x09 "t" (le64 1700000000000)]
          parseBson doc @?= Right (HM.fromList [("t", BsonUtcTime 1700000000000)])

      , testCase "Binary" $ do
          let payload = [0x01, 0x02, 0x03]
          let doc = mkDoc [elem' 0x05 "d" (le32 3 ++ [0x00] ++ payload)]
          parseBson doc @?= Right (HM.fromList [("d", BsonBinary 0x00 (BS.pack payload))])

      , testCase "Binary with custom subtype" $ do
          let payload = [0xAB, 0xCD]
          let doc = mkDoc [elem' 0x05 "d" (le32 2 ++ [0x80] ++ payload)]
          parseBson doc @?= Right (HM.fromList [("d", BsonBinary 0x80 (BS.pack payload))])
      ]

  , testGroup "Document structure"
      [ testCase "empty document" $ do
          parseBson (BS.pack (le32 5 ++ [0x00])) @?= Right HM.empty

      , testCase "multiple fields" $ do
          let doc = mkDoc [ elem' 0x10 "a" (le32 1)
                          , elem' 0x10 "b" (le32 2)
                          , elem' 0x10 "c" (le32 3)
                          ]
          parseBson doc @?= Right (HM.fromList
            [ ("a", BsonInt32 1)
            , ("b", BsonInt32 2)
            , ("c", BsonInt32 3)
            ])

      , testCase "mixed-type fields" $ do
          let doc = mkDoc [ elem' 0x10 "i"   (le32 7)
                          , elem' 0x02 "s"   (bsonStr "hi")
                          , elem' 0x08 "ok"  [0x01]
                          , elem' 0x0A "nil" []
                          ]
          parseBson doc @?= Right (HM.fromList
            [ ("i",   BsonInt32 7)
            , ("s",   BsonString (T.pack "hi"))
            , ("ok",  BsonBool True)
            , ("nil", BsonNull)
            ])

      , testCase "embedded document single field" $ do
          let innerBytes = BS.unpack (mkDoc [elem' 0x10 "x" (le32 99)])
          let doc = mkDoc [elem' 0x03 "inner" innerBytes]
          parseBson doc @?= Right (HM.fromList [("inner", BsonObject (HM.fromList [("x", BsonInt32 99)]))])

      , testCase "embedded document multiple fields" $ do
          let innerBytes = BS.unpack $ mkDoc
                [ elem' 0x10 "a" (le32 1)
                , elem' 0x02 "b" (bsonStr "hi")
                , elem' 0x08 "c" [0x01]
                ]
          let doc = mkDoc [elem' 0x03 "obj" innerBytes]
          parseBson doc @?= Right (HM.fromList
            [ ("obj", BsonObject (HM.fromList
                [ ("a", BsonInt32 1)
                , ("b", BsonString "hi")
                , ("c", BsonBool True)
                ]))
            ])

      , testCase "empty embedded document" $ do
          let innerBytes = BS.unpack (BS.pack (le32 5 ++ [0x00]))
          let doc = mkDoc [elem' 0x03 "obj" innerBytes]
          parseBson doc @?= Right (HM.fromList [("obj", BsonObject HM.empty)])

      , testCase "deeply nested document" $ do
          let level2 = BS.unpack (mkDoc [elem' 0x10 "z" (le32 42)])
          let level1 = BS.unpack (mkDoc [elem' 0x03 "inner" level2])
          let doc    = mkDoc [elem' 0x03 "outer" level1]
          parseBson doc @?= Right (HM.fromList
            [ ("outer", BsonObject (HM.fromList
                [ ("inner", BsonObject (HM.fromList
                    [ ("z", BsonInt32 42) ]))
                ]))
            ])

      , testCase "array of ints" $ do
          let arrBytes = BS.unpack $ mkDoc
                [ elem' 0x10 "0" (le32 10)
                , elem' 0x10 "1" (le32 20)
                , elem' 0x10 "2" (le32 30)
                ]
          let doc = mkDoc [elem' 0x04 "arr" arrBytes]
          parseBson doc @?= Right
            (HM.fromList [("arr", BsonArray [BsonInt32 10, BsonInt32 20, BsonInt32 30])])

      , testCase "array of strings" $ do
          let arrBytes = BS.unpack $ mkDoc
                [ elem' 0x02 "0" (bsonStr "a")
                , elem' 0x02 "1" (bsonStr "b")
                ]
          let doc = mkDoc [elem' 0x04 "strs" arrBytes]
          parseBson doc @?= Right
            (HM.fromList [("strs", BsonArray [BsonString (T.pack "a"), BsonString (T.pack "b")])])

      , testCase "empty array" $ do
          let arrBytes = BS.unpack (BS.pack (le32 5 ++ [0x00]))
          let doc = mkDoc [elem' 0x04 "arr" arrBytes]
          parseBson doc @?= Right (HM.fromList [("arr", BsonArray [])])
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

  , testGroup "Getters"
      [ testCase "get existing key" $
          get "a" (HM.fromList [("a", BsonInt32 1)]) @?= Just (BsonInt32 1)

      , testCase "get missing key" $
          get "z" (HM.fromList [("a", BsonInt32 1)]) @?= Nothing

      , testCase "getString hit" $
          getString "s" (HM.fromList [("s", BsonString "hi")]) @?= Just "hi"

      , testCase "getString type mismatch" $
          getString "s" (HM.fromList [("s", BsonInt32 1)]) @?= Nothing

      , testCase "getInt32 hit" $
          getInt32 "n" (HM.fromList [("n", BsonInt32 42)]) @?= Just 42

      , testCase "getInt64 hit" $
          getInt64 "n" (HM.fromList [("n", BsonInt64 1000000000000)]) @?= Just 1000000000000

      , testCase "getDouble hit" $
          getDouble "d" (HM.fromList [("d", BsonDouble 3.14)]) @?= Just 3.14

      , testCase "getBool hit true" $
          getBool "b" (HM.fromList [("b", BsonBool True)]) @?= Just True

      , testCase "getBool type mismatch" $
          getBool "b" (HM.fromList [("b", BsonInt32 1)]) @?= Nothing

      , testCase "getArray hit" $
          getArray "xs" (HM.fromList [("xs", BsonArray [BsonInt32 1, BsonInt32 2])])
            @?= Just [BsonInt32 1, BsonInt32 2]

      , testCase "getObject hit" $
          getObject "o" (HM.fromList [("o", BsonObject (HM.fromList [("x", BsonInt32 9)]))])
            @?= Just (HM.fromList [("x", BsonInt32 9)])

      , testCase "getObject type mismatch" $
          getObject "o" (HM.fromList [("o", BsonInt32 1)]) @?= Nothing

      , testCase "getUtcTime hit" $
          getUtcTime "t" (HM.fromList [("t", BsonUtcTime 0)]) @?= Just 0

      , testCase "getBinary hit" $
          getBinary "d" (HM.fromList [("d", BsonBinary 0x00 (BS.pack [1,2,3]))])
            @?= Just (0x00, BS.pack [1,2,3])

      , testCase "getPath one level" $
          getPath ["a"] (HM.fromList [("a", BsonInt32 7)]) @?= Just (BsonInt32 7)

      , testCase "getPath two levels" $
          getPath ["outer", "inner"]
            (HM.fromList [("outer", BsonObject (HM.fromList [("inner", BsonInt32 42)]))])
            @?= Just (BsonInt32 42)

      , testCase "getPath missing intermediate" $
          getPath ["x", "y"] (HM.fromList [("x", BsonInt32 1)]) @?= Nothing

      , testCase "getPath empty" $
          getPath [] (HM.fromList [("a", BsonInt32 1)]) @?= Nothing
      ]

  , testGroup "mergeValues"
      [ testCase "Int32 adds" $
          mergeValues (BsonInt32 3) (BsonInt32 7) @?= BsonInt32 10

      , testCase "Int64 adds" $
          mergeValues (BsonInt64 1000000000) (BsonInt64 2000000000)
            @?= BsonInt64 3000000000

      , testCase "Double adds" $
          mergeValues (BsonDouble 1.5) (BsonDouble 2.5) @?= BsonDouble 4.0

      , testCase "String concatenates" $
          mergeValues (BsonString "foo") (BsonString "bar") @?= BsonString "foobar"

      , testCase "Bool or" $
          mergeValues (BsonBool False) (BsonBool True) @?= BsonBool True

      , testCase "Bool or both false" $
          mergeValues (BsonBool False) (BsonBool False) @?= BsonBool False

      , testCase "Array concatenates" $
          mergeValues (BsonArray [BsonInt32 1]) (BsonArray [BsonInt32 2])
            @?= BsonArray [BsonInt32 1, BsonInt32 2]

      , testCase "Null merges to Null" $
          mergeValues BsonNull BsonNull @?= BsonNull

      , testCase "Object merges recursively" $
          mergeValues
            (BsonObject (HM.fromList [("a", BsonInt32 1), ("b", BsonInt32 2)]))
            (BsonObject (HM.fromList [("b", BsonInt32 10), ("c", BsonInt32 3)]))
            @?= BsonObject (HM.fromList [("a", BsonInt32 1), ("b", BsonInt32 12), ("c", BsonInt32 3)])

      , testCase "type mismatch throws" $ do
          result <- try (evaluate (mergeValues (BsonInt32 1) (BsonString "x"))) :: IO (Either ErrorCall BsonValue)
          case result of
            Left  _ -> return ()
            Right _ -> assertFailure "expected error on type mismatch"
      ]

  , testGroup "merge"
      [ testCase "disjoint keys" $
          merge (HM.fromList [("a", BsonInt32 1)])
                (HM.fromList [("b", BsonInt32 2)])
            @?= HM.fromList [("a", BsonInt32 1), ("b", BsonInt32 2)]

      , testCase "shared key combines values" $
          merge (HM.fromList [("n", BsonInt32 3)])
                (HM.fromList [("n", BsonInt32 7)])
            @?= HM.fromList [("n", BsonInt32 10)]

      , testCase "left-only keys preserved" $
          merge (HM.fromList [("a", BsonInt32 1), ("b", BsonBool True)])
                (HM.fromList [("a", BsonInt32 9)])
            @?= HM.fromList [("a", BsonInt32 10), ("b", BsonBool True)]

      , testCase "merge empty left" $
          merge HM.empty (HM.fromList [("x", BsonInt32 1)])
            @?= HM.fromList [("x", BsonInt32 1)]

      , testCase "merge empty right" $
          merge (HM.fromList [("x", BsonInt32 1)]) HM.empty
            @?= HM.fromList [("x", BsonInt32 1)]

      , testCase "nested objects merged recursively" $
          merge
            (HM.fromList [("o", BsonObject (HM.fromList [("x", BsonInt32 1)]))])
            (HM.fromList [("o", BsonObject (HM.fromList [("y", BsonInt32 2)]))])
            @?= HM.fromList [("o", BsonObject (HM.fromList [("x", BsonInt32 1), ("y", BsonInt32 2)]))]
      ]
  ]
