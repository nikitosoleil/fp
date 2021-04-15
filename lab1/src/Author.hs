module Author where

import CommonEntity
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data Author = Author
  { uid :: Maybe Integer,
    name :: String,
    surname :: String
  }

createAuthor :: IO Author
createAuthor = do
  putStrLn "Enter author name:"
  _name <- getLine
  putStrLn "Enter author surname:"
  _surname <- getLine
  return Author
    {
      uid = Nothing,
      name = _name,
      surname = _surname
  }

updateAuthor :: IO Author
updateAuthor = do
  putStrLn "Enter author uid:"
  _uid <- getLine
  putStrLn "Enter author name:"
  _name <- getLine
  putStrLn "Enter author surname:"
  _surname <- getLine
  return Author
    {
      uid = Just (read _uid :: Integer),
      name = _name,
      surname = _surname
  }

printAuthor :: Author -> IO ()
printAuthor author = do
  putStrLn "Author:"
  putStr (" Id: " ++ show (uid author))
  putStr (" Name: " ++ name author)
  putStr (" Surname: " ++ surname author)
  putStrLn ""

instance CommonEntity Author where
  getTableName = do "authors"

  convert [SqlInteger _id, SqlByteString _name, SqlByteString _surname] =
      Author
        { uid = Just _id,
          name = BS.unpack _name,
          surname = BS.unpack _surname
        }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from authors"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from authors where id = ?"

  removeAll conn _ = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from authors"

  removeById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from authors where id = ?"

  add conn entity =  do
    _ <- run conn query [SqlString (name entity), SqlString (surname entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into authors (name, surname) values (?, ?)"
      lastId = "select * from authors order by id desc limit 1"
