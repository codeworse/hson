{-# LANGUAGE LambdaCase #-}
module Hson.Get where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word8)
import Hson.Parse (BsonValue(..), Docs)
import qualified Data.HashMap.Strict as HM

get :: Text -> Docs -> Maybe BsonValue
get key docs = HM.lookup key docs

getString  :: Text -> Docs -> Maybe Text
getString key docs = get key docs >>= \case
    BsonString s -> Just s
    _            -> Nothing

getInt32   :: Text -> Docs -> Maybe Int32
getInt32 key docs = get key docs >>= \case
    BsonInt32 s -> Just s
    _            -> Nothing

getInt64   :: Text -> Docs -> Maybe Int64
getInt64 key docs = get key docs >>= \case
    BsonInt64 s -> Just s
    _            -> Nothing

getDouble  :: Text -> Docs -> Maybe Double
getDouble key docs = get key docs >>= \case
    BsonDouble s -> Just s
    _            -> Nothing

getBool    :: Text -> Docs -> Maybe Bool
getBool key docs = get key docs >>= \case
    BsonBool s -> Just s
    _            -> Nothing

getArray   :: Text -> Docs -> Maybe [BsonValue]
getArray key docs = get key docs >>= \case
    BsonArray s -> Just s
    _            -> Nothing

getObject  :: Text -> Docs -> Maybe Docs
getObject key docs = get key docs >>= \case
    BsonObject s -> Just s
    _            -> Nothing

getUtcTime :: Text -> Docs -> Maybe Int64
getUtcTime key docs = get key docs >>= \case
    BsonUtcTime s -> Just s
    _            -> Nothing

getBinary  :: Text -> Docs -> Maybe (Word8, ByteString)
getBinary key docs = get key docs >>= \case
    BsonBinary sub bs -> Just (sub, bs)
    _                 -> Nothing

getPath :: [Text] -> Docs -> Maybe BsonValue
getPath []     _    = Nothing
getPath [k]    docs = get k docs
getPath (k:ks) docs = getObject k docs >>= getPath ks
