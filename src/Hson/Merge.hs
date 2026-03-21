module Hson.Merge where

import Hson.Parse (BsonValue(..), Docs)
import qualified Data.HashMap.Strict as HM


mergeValues :: BsonValue -> BsonValue -> BsonValue
mergeValues (BsonObject  a) (BsonObject  b) = BsonObject (HM.unionWith mergeValues a b)
mergeValues (BsonArray   a) (BsonArray   b) = BsonArray (a <> b)
mergeValues (BsonString  a) (BsonString  b) = BsonString (a <> b)
mergeValues (BsonInt32   a) (BsonInt32   b) = BsonInt32 (a + b)
mergeValues (BsonInt64   a) (BsonInt64   b) = BsonInt64 (a + b)
mergeValues (BsonDouble  a) (BsonDouble  b) = BsonDouble (a + b)
mergeValues (BsonBool    a) (BsonBool    b) = BsonBool (a || b)
mergeValues (BsonUtcTime a) (BsonUtcTime b) = BsonUtcTime (a + b)
mergeValues BsonNull         BsonNull       = BsonNull
mergeValues _                _              = error "Cannot merge bson values"  -- type mismatch or binary


merge :: Docs -> Docs -> Docs
merge leftDocs rightDocs = HM.unionWith mergeValues leftDocs rightDocs
