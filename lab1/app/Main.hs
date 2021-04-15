module Main where

import CommonEntity
import Data.List
import Database.HDBC
import Database.HDBC.PostgreSQL

import Author
import Software
import User
import Type
import UsageStatistics

--TODO: move to utils
tables :: [[Char]]
tables = ["software", "authors", "users", "types", "usage_statistics"]

main :: IO ()
main = do
  c <- connectPostgreSQL "host=queenie.db.elephantsql.com dbname=papiffdi user=papiffdi password=t8X7c_ricaLtCEHHBKFL73t-ETseVptU"
  putStrLn "\nChoose entity:"
  putStrLn (intercalate "\n" tables)
  putStrLn "else exit\n"
  _name <- getLine
  putStrLn ("you said: " ++ _name)
  if _name `elem` tables
    then do
      putStrLn "\nChoose action:\nn - new\nu - update\nd - delete\ndi - delete by id\ng - get all\ngi - get by id\nexit"
      action <- getLine
      putStrLn ("you said: " ++ action)
      if action == "exit"
        then do
          putStrLn "Unknown command"
          disconnect c
          main
        else do
          case action of
            "n" -> do
              putStrLn ("New " ++ _name)
              case _name of
                "software" -> do
                  software <- createSoftware
                  result <- add c software :: IO Software
                  printSoftware result
                "authors" -> do
                  author <- createAuthor
                  result <- add c author :: IO Author
                  printAuthor result
                "users" -> do
                  user <- createUser
                  result <- add c user :: IO User
                  printUser result
                "types" -> do
                  _type <- createType
                  result <- add c _type :: IO Type
                  printType result
                "usage_statistics" -> do
                  usage_statistics <- createUsageStatistics
                  result <- add c usage_statistics :: IO UsageStatistics
                  printUsageStatistics result
              commit c
--            "d" -> do
--              putStrLn ("Delete " ++ _name)
--              putStrLn ("Get by id" ++ _name)
--              putStrLn "Enter id:"
--              _uid <- getLine
--              let _id = (read _uid :: Integer)
--              result <- removeAll c :: IO Bool
--              putStrLn "Deleted"
--            "di" -> do
--              putStrLn ("Delete by id " ++ _name)
            "g" -> do
              putStrLn ("Get all " ++ _name)
              case _name of
                "software" -> do
                  result <- getAll c :: IO [Software]
                  mapM_ printSoftware result
                "authors" -> do
                  result <- getAll c :: IO [Author]
                  mapM_ printAuthor result
                "users" -> do
                  result <- getAll c :: IO [User]
                  mapM_ printUser result
                "types" -> do
                  result <- getAll c :: IO [Type]
                  mapM_ printType result
                "usage_statistics" -> do
                  result <- getAll c :: IO [UsageStatistics]
                  mapM_ printUsageStatistics result
            "gi" -> do
              putStrLn ("Get by id" ++ _name)
              putStrLn "Enter id:"
              _uid <- getLine
              let _id = (read _uid :: Integer)
              case _name of
                "software" -> do
                  result <- getById c _id :: IO (Maybe Software)
                  mapM_ printSoftware result
                "authors" -> do
                  result <- getById c _id :: IO (Maybe Author)
                  mapM_ printAuthor result
                "users" -> do
                  result <- getById c _id :: IO (Maybe User)
                  mapM_ printUser result
                "types" -> do
                  result <- getById c _id :: IO (Maybe Type)
                  mapM_ printType result
                "usage_statistics" -> do
                  result <- getById c _id :: IO (Maybe UsageStatistics)
                  mapM_ printUsageStatistics result
            _ -> do
              putStrLn "Unknown command"
              disconnect c
              main
          disconnect c
          main
  else if _name /= "exit"
    then putStrLn "Unknown command"
    else putStrLn "Exit"
