module Software where

import CommonEntity
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Software = Software
  { uid :: Maybe Integer,
    name :: String,
    annotation :: String,
    version :: Integer,
    datetime_start :: String,
    datetime_end :: String,
    author_id :: Integer,
    type_id :: Integer
  }

createSoftware :: IO Software
createSoftware = do
  putStrLn "Enter software name:"
  _name <- getLine
  putStrLn "Enter software annotation:"
  _annotation <- getLine
  putStrLn "Enter software version:"
  _version <- getLine
  putStrLn "Enter software datetime_start:"
  _datetime_start <- getLine
  putStrLn "Enter software datetime_end:"
  _datetime_end <- getLine
  putStrLn "Enter software author_id:"
  _author_id <- getLine
  putStrLn "Enter software type_id:"
  _type_id <- getLine
  return
    Software
      { uid = Nothing,
        name = _name,
        annotation = _annotation,
        version = read _version :: Integer,
        datetime_start = _datetime_start,
        datetime_end = _datetime_end,
        author_id = read _author_id :: Integer,
        type_id = read _type_id :: Integer
      }

updateSoftware :: IO Software
updateSoftware = do
  do putStrLn "Enter software id:"
  _uid <- getLine
  putStrLn "Enter software name:"
  _name <- getLine
  putStrLn "Enter software annotation:"
  _annotation <- getLine
  putStrLn "Enter software version:"
  _version <- getLine
  putStrLn "Enter software datetime_start:"
  _datetime_start <- getLine
  putStrLn "Enter software datetime_end:"
  _datetime_end <- getLine
  putStrLn "Enter software author_id:"
  _author_id <- getLine
  putStrLn "Enter software type_id:"
  _type_id <- getLine
  return
    Software
      { uid = Just (read _uid :: Integer),
        name = _name,
        annotation = _annotation,
        version = read _version :: Integer,
        datetime_start = _datetime_start,
        datetime_end = _datetime_end,
        author_id = read _author_id :: Integer,
        type_id = read _type_id :: Integer
      }

printSoftware :: Software -> IO ()
printSoftware software = do
  putStrLn "Software:"
  putStr (" Id: " ++ show (uid software))
  putStr (" Name: " ++ name software)
  putStr (" Annotation: " ++ annotation software)
  putStr (" Version: " ++ show (version software))
  putStr (" Datetime_start: " ++ datetime_start software)
  putStr (" Datetime_end: " ++ datetime_end software)
  putStr (" Author_id: " ++ show (author_id software))
  putStr (" Type_id: " ++ show (type_id software))
  putStrLn ""

instance CommonEntity Software where
  getTableName = "software"

  convert [SqlInteger _id, SqlByteString _name, SqlByteString _annotation, SqlInteger _version, SqlByteString _datetime_start, SqlByteString _datetime_end, SqlInteger _author_id, SqlInteger _type_id] =
    Software
      { uid = Just _id,
        name = BS.unpack _name,
        annotation = BS.unpack _annotation,
        version = _version,
        datetime_start = BS.unpack _datetime_start,
        datetime_end = BS.unpack _datetime_end,
        author_id = _author_id,
        type_id = _type_id
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from software"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from software where id = ?"

  removeAll conn _ = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from software"

  removeById conn _uid _ = do
    changed <- run conn query [SqlInteger _uid]
    return $ changed == 1
    where
      query = "delete from software where id = ?"

  add conn entity = do
    _ <- run conn query [SqlString (name entity), SqlString (annotation entity), SqlInteger (version entity),SqlString (datetime_start entity), SqlString (datetime_end entity), SqlInteger (author_id entity), SqlInteger (type_id entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into software (name, annotation, version, datetime_start, datetime_end, author_id, type_id) values (?, ?, ?, ?, ?, ?, ?)"
      lastId = "select * from software order by id desc limit 1"
