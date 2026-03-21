module Hson where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Word (Word8)
import Data.Binary.Get
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

data BsonValue
  = BsonDouble   Double
  | BsonString   Text
  | BsonDoc      Doc
  | BsonArray    [BsonValue]
  | BsonBinary   Word8 ByteString
  | BsonBool     Bool
  | BsonUtcTime  Int64
  | BsonNull
  | BsonInt32    Int32
  | BsonInt64    Int64
  deriving (Show, Eq)

type Doc  = (Text, BsonValue)
type Docs = [Doc]

parseElement :: Get Doc
parseElement = do
    typeByte <- getWord8
    key      <- parseCString
    value    <- parseValue typeByte
    return (key, value)

parseCString :: Get Text
parseCString = do
    bytes <- getLazyByteStringNul
    return $ TE.decodeUtf8 (BL.toStrict bytes)

parseValue :: Word8 -> Get BsonValue
parseValue typeByte = case typeByte of
    0x01 -> BsonDouble  <$> getDoublele
    0x02 -> BsonString  <$> parseBsonString
    0x03 -> BsonDoc     <$> parseDocument
    0x04 -> BsonArray   <$> parseArray
    0x05 -> parseBinary
    0x08 -> BsonBool    <$> parseBool
    0x09 -> BsonUtcTime <$> getInt64le
    0x0A -> return BsonNull
    0x10 -> BsonInt32   <$> getInt32le
    0x12 -> BsonInt64   <$> getInt64le
    _    -> fail $ "Unknown BSON type: " <> show typeByte

parseBsonString :: Get Text
parseBsonString = do
    len   <- getInt32le
    bytes <- getByteString (fromIntegral len - 1)
    _null <- getWord8
    return $ TE.decodeUtf8 bytes

parseBool :: Get Bool
parseBool = do
    b <- getWord8
    return (b == 0x01)

parseBinary :: Get BsonValue
parseBinary = do
    len     <- getInt32le
    subtype <- getWord8
    bytes   <- getByteString (fromIntegral len)
    return $ BsonBinary subtype bytes

parseArray :: Get [BsonValue]
parseArray = map snd <$> parseDocumentBody

parseDocument :: Get Doc
parseDocument = do
    docs <- parseDocumentBody
    case docs of
        []        -> fail "Empty document"
        (doc : _) -> return doc

parseDocumentBody :: Get Docs
parseDocumentBody = do
    size <- getInt32le
    let bodyLen = fromIntegral size - 4 - 1
    body <- getLazyByteString bodyLen
    term <- getWord8
    if term /= 0x00
        then fail "Document terminator is not 0x00"
        else case runGetOrFail parseElements body of
               Left  (_, _, err) -> fail err
               Right (_, _, els) -> return els

parseElements :: Get Docs
parseElements = do
    empty <- isEmpty
    if empty
        then return []
        else do
            el   <- parseElement
            rest <- parseElements
            return (el : rest)

parseBson :: ByteString -> Either String Docs
parseBson bs =
    case runGetOrFail parseDocumentBody (BL.fromStrict bs) of
        Left  (_, _, err)    -> Left err
        Right (_, _, result) -> Right result

