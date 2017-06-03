module Order(
  Order,
  Currency,
  Direction,
  getOrderFromLine
) where

import qualified Data.List as List
import Text.Regex (splitRegex, mkRegex)


data Currency = EUR | GBP | USD deriving (Show)
data Direction = BUY | SELL deriving (Show)
data Order = Order {timestamp :: Int, currency :: Currency, quantity :: Int, orderType :: Direction}

instance Show Order where
  show (Order timeStamp currency quantity orderType) = 
    show currency ++ " " ++ show quantity ++ " " ++ show orderType

getOrderFromLine :: String -> Order
getOrderFromLine aCsvLine = Order orderTime orderCurrency orderQuantity orderType
  where 
    orderParams = splitRegex (mkRegex ",") aCsvLine
    orderTime = read $ orderParams !! 0 :: Int
    orderCurrency = case orderParams !! 1 of 
      "EUR" -> EUR
      "GBP" -> GBP
      "USD" -> USD
    orderQuantity = read $ orderParams !! 2 :: Int
    orderType = case orderParams !! 3 of 
      "BUY" -> BUY
      "SELL" -> SELL
