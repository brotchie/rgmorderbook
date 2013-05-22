module Data.OrderBook.RGM (
    parseLogFile,
    Timestamp,
    OrderId,
    Price,
    Size,
    Side (..),
    LogEntry (..),
) where

import Data.Maybe

-- Haskell strings are unicode linked lists; not
-- good for performance. For this reason we use
-- raw ByteString objects. Around 50x faster.
import qualified Data.ByteString.Lex.Double as LD
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
 
type Timestamp = Int
 
type OrderId = ByteString
 
type Price = Double
 
type Size = Int
 
data Side = Bid
          | Ask
          deriving (Show, Eq)
 
data LogEntry = AddOrder{timestamp :: Timestamp, orderid :: OrderId,
                         side :: Side, price :: Price, size :: Size}
              | ReduceOrder{timestamp :: Timestamp, orderid :: OrderId, size :: Size}
              deriving Show
 
readInt :: ByteString -> Int
readInt = fst . fromJust . B.readInt

readDouble :: ByteString -> Double
readDouble = fst . fromJust . LD.readDouble

readSide :: ByteString -> Side
readSide x
  = case B.head x of
        'B' -> Bid
        'S' -> Ask

parseLogEntry :: [ByteString] -> Maybe LogEntry
parseLogEntry [timestamp, _, orderid, side, price, size]
  = Just $ AddOrder (readInt timestamp) orderid (readSide side)
      (readDouble price)
      (readInt size)
parseLogEntry [timestamp, _, orderid, size]
  = Just $ ReduceOrder (readInt timestamp) orderid (readInt size)
parseLogEntry _ = Nothing

dot = (.) . (.)
infixr 8 `dot`

-- Faster than base mapMaybe.
mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' = map fromJust . filter isJust `dot` map 

parseLogFile :: ByteString -> [LogEntry]
parseLogFile content = mapMaybe' (parseLogEntry . B.words) $ B.lines content

