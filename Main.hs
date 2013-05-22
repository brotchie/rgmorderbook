module Main where

import Text.Printf
import Data.List
import Data.Maybe
import Data.OrderBook.RGM
import Control.Monad
import qualified Data.ByteString.Char8 as C

import qualified Data.Map as Map
import Data.Map ((!), empty, toDescList, toAscList, insertWith, update, updateLookupWithKey)

type OrderBookSide = Map.Map Price Size

data OrderBook = OrderBook { orders :: Map.Map OrderId (Side, Price, Size)
                           , bids :: OrderBookSide
                           , asks :: OrderBookSide
                           }
               deriving Show

targetSize :: Size
targetSize = 200

reduceOrder :: OrderId -> Size -> OrderBook -> OrderBook
reduceOrder oid subsize book = OrderBook orders' bids' asks'
    where (Just (side, price, _), orders' ) = updateLookupWithKey updateSize oid $ orders book
          updateSize k (side, price, size) = case size - subsize of
                                                r | r <= 0 -> Nothing
                                                r -> Just (side, price, r)
          bids' = updateIf Bid $ bids book
          asks' = updateIf Ask $ asks book
          updateIf s = if side == s then update updatePrice price
                                    else id
          updatePrice sz = case sz - subsize of
                                r | r <= 0 -> Nothing
                                r -> Just r

addOrder :: OrderId -> Side -> Price -> Size -> OrderBook -> OrderBook
addOrder oid side price size book = OrderBook orders' bids' asks'
    where orders' = Map.insert oid (side, price, size) $ orders book
          bids' = updateIf Bid $ bids book
          asks' = updateIf Ask $ asks book
          updateIf s = if side == s then insertWith (+) price size
                                    else id

emptyOrderBook :: OrderBook
emptyOrderBook = OrderBook empty empty empty

executeLogEntry :: OrderBook -> LogEntry -> OrderBook
executeLogEntry book (AddOrder timestamp oid side price size) = addOrder oid side price size book
executeLogEntry book (ReduceOrder timestamp oid size) = reduceOrder oid size book

calculateExpense :: Size -> OrderBook -> Maybe Price
calculateExpense size book = walkBook size 0 askList
    where askList = toAscList $ asks book

calculateIncome :: Size -> OrderBook -> Maybe Price
calculateIncome size book = walkBook size 0 bidList
    where bidList = toDescList $ bids book

walkBook :: Size -> Price -> [(Price, Size)] -> Maybe Price
walkBook rem expense ((bprice, bsize):xs) = 
    if residual <= 0 then Just $ expense + bprice * fromIntegral rem
                     else walkBook residual (expense + bprice * fromIntegral bsize) xs
    where residual = rem - bsize
walkBook _ _ [] = Nothing

-- For each (price, size) pair subtract size from target and multiply by price.
-- Accumulate the total expense plus carry along the size remaining.

showAmount :: Maybe Price -> String
showAmount (Just x) = printf "%.2f" x
showAmount Nothing = "NA"

runBook :: [LogEntry] -> IO ()
runBook entries = do
    executeLogEntries Nothing Nothing emptyOrderBook entries
    --putStrLn $ show $ foldl' executeLogEntry book entries
    where executeLogEntries pbuy psell book (x:xs) = do
            let book' = executeLogEntry book x
                pbuy' = calculateExpense targetSize book'
                psell' = calculateIncome targetSize book'

            when (psell /= psell') $ putStrLn $ show (timestamp x) ++ " S " ++ (showAmount psell')
            when (pbuy /= pbuy') $ putStrLn $ show (timestamp x) ++ " B " ++ (showAmount pbuy')
            --putStrLn $ show $ (Map.size . orders) book'
            --putStrLn $ show $ calculateExpenseForSize targetSize book'
            --putStrLn $ show (timestamp x) ++ 
            --           " Income: " ++ (show $ calculateIncome targetSize book') ++ 
            --           " Expense: " ++ (show $ calculateExpense targetSize book')
            executeLogEntries pbuy' psell' book' xs
          executeLogEntries _ _ _ [] = return ()

main = do content <- C.readFile "pricer.in"
          runBook $ parseLogFile content 
          --mapM_ (C.putStrLn . orderid) $ parseLogFile content
