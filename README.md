# hson

A Haskell library for parsing and merging [BSON](https://bsonspec.org/) documents.

## Types

```haskell
type Docs = HashMap Text BsonValue

data BsonValue
  = BsonDouble   Double
  | BsonString   Text
  | BsonObject   Docs        -- nested document
  | BsonArray    [BsonValue]
  | BsonBinary   Word8 ByteString
  | BsonBool     Bool
  | BsonUtcTime  Int64       -- milliseconds since Unix epoch
  | BsonNull
  | BsonInt32    Int32
  | BsonInt64    Int64
```

## Parsing

```haskell
import Hson.Parse (parseBson)

-- parse a raw BSON ByteString into a document
case parseBson bytes of
  Left  err -> putStrLn ("parse error: " ++ err)
  Right doc -> print doc
```

## Getting values

```haskell
import Hson.Get

let doc = parseBson bytes

-- typed getters return Nothing on missing key or type mismatch
getString  "name"  doc  -- Maybe Text
getInt32   "age"   doc  -- Maybe Int32
getBool    "active" doc -- Maybe Bool
getObject  "address" doc -- Maybe Docs

-- traverse nested objects with a key path
getPath ["user", "address", "city"] doc -- Maybe BsonValue
```

## Merging

```haskell
import Hson.Merge

-- merge two documents; shared keys are combined with mergeValues
let merged = merge doc1 doc2
```

`mergeValues` combines same-type values:

| Type         | Behaviour              |
|--------------|------------------------|
| `BsonInt32`  | addition               |
| `BsonInt64`  | addition               |
| `BsonDouble` | addition               |
| `BsonString` | concatenation          |
| `BsonBool`   | logical or             |
| `BsonArray`  | concatenation          |
| `BsonObject` | recursive merge        |
| `BsonNull`   | `BsonNull`             |
| type mismatch | runtime error         |

### Example

```haskell
let a = HM.fromList [("score", BsonInt32 10), ("tags", BsonArray [BsonString "haskell"])]
let b = HM.fromList [("score", BsonInt32 5),  ("tags", BsonArray [BsonString "bson"])]

merge a b
-- { "score": BsonInt32 15
-- , "tags":  BsonArray [BsonString "haskell", BsonString "bson"]
-- }
```

### Nested merge

```haskell
let a = HM.fromList [("user", BsonObject (HM.fromList [("name", BsonString "Alice")]))]
let b = HM.fromList [("user", BsonObject (HM.fromList [("age",  BsonInt32 30)]))]

merge a b
-- { "user": BsonObject { "name": BsonString "Alice", "age": BsonInt32 30 } }
```
