module Data.OrderBook.RGM (
    parseLogFile,
    LogEntry,
    timestamp,
    orderid,
    side,
    price,
    size
) where

import Data.Maybe

-- Haskell strings are unicode linked lists; not
-- good for performance. For this reason we use
-- raw ByteString objects. Around 50x faster.
import qualified Data.ByteString.Lex.Double as LD
import qualified Data.ByteString.Char8 as C
 
type Timestamp = Int
 
type OrderId = C.ByteString
 
type Price = Double
 
type Size = Int
 
data Side = Bid
          | Ask
          deriving Show
 
data LogEntry = AddOrder{timestamp :: Timestamp, orderid :: OrderId,
                         side :: Side, price :: Price, size :: Size}
              | ReduceOrder{timestamp :: Timestamp, orderid :: OrderId, size :: Size}
              deriving Show
 
readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

readDouble :: C.ByteString -> Double
readDouble = fst . fromJust . LD.readDouble

readSide :: C.ByteString -> Side
readSide x
  = case C.head x of
        'B' -> Bid
        'S' -> Ask


parseLogEntry :: [C.ByteString] -> Maybe LogEntry
parseLogEntry [timestamp, _, orderid, side, price, size]
  = Just $ AddOrder (readInt timestamp) orderid (readSide side)
      (readDouble price)
      (readInt size)
parseLogEntry [timestamp, _, orderid, size]
  = Just $ ReduceOrder (readInt timestamp) orderid (readInt size)
parseLogEntry _ = Nothing

dot = ((.) . (.))
infixr 8 `dot`

-- Faster than base mapMaybe.
mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' = map fromJust . filter isJust `dot` map 

parseLogFile :: C.ByteString -> [LogEntry]
parseLogFile content = mapMaybe' (parseLogEntry . C.words) $ C.lines content

