module UsageStatistics where

import CommonEntity
import qualified Data.ByteString.Char8 as BS
import Database.HDBC

data UsageStatistics = UsageStatistics
  { uid :: Maybe Integer,
    software_id :: Integer,
    user_id :: Integer,
    quantity :: Integer
  }

createUsageStatistics :: IO UsageStatistics
createUsageStatistics = do
  putStrLn "Enter usage statistics software_id:"
  _software_id <- getLine
  putStrLn "Enter usage statistics user_id:"
  _user_id <- getLine
  putStrLn "Enter usage statistics quantity:"
  _quantity <- getLine
  return
    UsageStatistics
      { uid = Nothing,
        software_id = read _software_id :: Integer,
        user_id = read _user_id :: Integer,
        quantity = read _quantity :: Integer
      }

updateUsageStatistics :: IO UsageStatistics
updateUsageStatistics = do
  putStrLn "Enter software id:"
  _uid <- getLine
  putStrLn "Enter author software_id:"
  _software_id <- getLine
  putStrLn "Enter author user_id:"
  _user_id <- getLine
  putStrLn "Enter author quantity:"
  _quantity <- getLine
  return
    UsageStatistics
      { uid = Just (read _uid :: Integer),
        software_id = read _software_id :: Integer,
        user_id = read _user_id :: Integer,
        quantity = read _quantity :: Integer
      }

printUsageStatistics :: UsageStatistics -> IO ()
printUsageStatistics usage_statistics = do
  putStrLn "Usage Statistics:"
  putStr (" Id: " ++ show (uid usage_statistics))
  putStr (" Software_id: " ++ show (software_id usage_statistics))
  putStr (" User_id: " ++ show (user_id usage_statistics))
  putStr (" Quantity: " ++ show (quantity usage_statistics))
  putStrLn ""

instance CommonEntity UsageStatistics where
  getTableName = do "usage_statistics"

  convert [SqlInteger _id, SqlInteger _software_id, SqlInteger _user_id, SqlInteger _quantity] =
    UsageStatistics
      { uid = Just _id,
        software_id = _software_id,
        user_id = _user_id,
        quantity = _quantity
      }
  convert x = error $ "Unexpected result: " ++ show x

  getAll conn = do
    result <- quickQuery' conn query []
    return $ map convert result
    where
      query = "select * from usage_statistics"

  getById conn _id = do
    result <- quickQuery' conn query [SqlInteger _id]
    let rows = map convert result
    return $
      if null rows
        then Nothing
        else Just (last rows)
    where
      query = "select * from usage_statistics where software_id = ?"

  removeAll conn _ = do
    changed <- run conn query []
    return $ changed == 1
    where
      query = "delete from ?"

  removeById conn _id _ = do
    changed <- run conn query [SqlInteger _id]
    return $ changed == 1
    where
      query = "delete from usage_statistics where software_id = ?"

  add conn entity = do
    _ <- run conn query [SqlInteger (software_id entity), SqlInteger (user_id entity), SqlInteger (quantity entity)]
    result <- quickQuery' conn lastId []
    let rows = map convert result
    return $ last rows
    where
      query = "insert into usage_statistics (software_id, user_id, quantity) values (?, ?, ?)"
      lastId = "select * from usage_statistics order by software_id desc limit 1"
