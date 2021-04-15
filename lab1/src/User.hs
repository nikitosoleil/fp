module User where

import CommonEntity
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data User = User
  { uid :: Maybe Integer,
    name :: String,
    surname :: String,
    start_date :: String
  }

createUser :: IO User
createUser = do
  putStrLn "Enter user name:"
  _name <- getLine
  putStrLn "Enter user surname:"
  _surname <- getLine
  putStrLn "Enter user start_date:"
  _start_date <- getLine
  return
    User
      { uid = Nothing,
        name = _name,
        surname = _surname,
        start_date = _start_date
      }

updateUser :: IO User
updateUser = do
  putStrLn "Enter software id:"
  _uid <- getLine
  putStrLn "Enter user name:"
  _name <- getLine
  putStrLn "Enter user surname:"
  _surname <- getLine
  putStrLn "Enter user start_date:"
  _start_date <- getLine
  return
    User
      { uid = Just (read _uid :: Integer),
        name = _name,
        surname = _surname,
        start_date = _start_date
      }

printUser :: User -> IO ()
printUser user = do
  putStrLn "User:"
  putStr (" Id: " ++ show (uid user))
  putStr (" Name: " ++ name user)
  putStr (" Surname: " ++ surname user)
  putStr (" Start_date: " ++ start_date user)
  putStrLn ""

instance CommonEntity User where
  getTableName = do "users"

  convert [SqlInteger _id, SqlByteString _name, SqlByteString _surname, SqlByteString _start_date] =
    User
      { uid = Just _id,
        name = BS.unpack _name,
        surname = BS.unpack _surname,
        start_date = BS.unpack _start_date
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from users"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from users where id = ?"

  removeAll conn _id = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from ?"

  removeById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from users where id = ?"

  add conn entity = do
    _ <- run conn query [SqlString (name entity), SqlString (surname entity), SqlString (start_date entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into users (name, surname, start_date) values (?, ?, ?)"
      lastId = "select * from users order by id desc limit 1"
