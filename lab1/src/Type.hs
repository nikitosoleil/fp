module Type where

import CommonEntity
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Type = Type
  { uid :: Maybe Integer,
    name :: String
  }
  
createType :: IO Type
createType = do
  putStrLn "Enter type name:"
  _name <- getLine
  return Type
    {
      uid = Nothing,
      name = _name
  }

updateType :: IO Type
updateType = do
  putStrLn "Enter software id:"
  _uid <- getLine
  putStrLn "Enter software name:"
  _name <- getLine
  return Type
    {
      uid = Just (read _uid :: Integer),
      name = _name
  }

printType :: Type -> IO ()
printType _type = do
  putStrLn "Type:"
  putStr (" Id: " ++ show (uid _type))
  putStr (" Name: " ++ name _type)
  putStrLn ""
  
instance CommonEntity Type where
  getTableName = "types"
  
  convert [SqlInteger _id, SqlByteString _name] =
        Type
          { uid = Just _id,
            name = BS.unpack _name
          }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from types"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from types where id = ?"

  removeAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from types"

  removeById conn _id _= do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from types where id = ?"

  add conn entity = do
    _ <- run conn query [SqlString (name entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into types (name) values (?)"
      lastId = "select * from types order by id desc limit 1"
