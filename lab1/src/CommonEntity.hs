{-# LANGUAGE AllowAmbiguousTypes #-}

module CommonEntity where

import Database.HDBC

class CommonEntity e where
  convert :: [SqlValue] -> e
  getTableName :: [Char]
  getAll :: IConnection c => c -> IO [e]
  getById :: IConnection c => c -> Integer -> IO (Maybe e)
  removeAll :: IConnection c => c -> e -> IO Bool
  removeById :: IConnection c => c -> Integer -> e -> IO Bool
  add :: IConnection c => c -> e -> IO e
