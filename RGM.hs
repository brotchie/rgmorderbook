module RGM (
    parseLogFile,
    LogEntry,
    timestamp,
    orderid,
    side,
    price,
    size
) where

import Data.Maybe
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
 
data Message = Add
             | Reduce

readInt = fst . fromJust . C.readInt
readDouble = fst . fromJust . LD.readDouble
readSide x
  = case C.head x of
        'B' -> Bid
        'S' -> Ask

parseAddOrder [timestamp, orderid, side, price, size]
  = AddOrder (readInt timestamp) orderid (readSide side)
      (readDouble price)
      (readInt size)

parseReduceOrder [timestamp, orderid, size]
  = ReduceOrder (readInt timestamp) orderid (readInt size)

parseLogLine line
  = case message of
        'A' -> parseAddOrder $ map ((!!) elements) [0, 2, 3, 4, 5]
        'R' -> parseReduceOrder $ map ((!!) elements) [0, 2, 3]
  where elements = C.words line
        message = C.head $ elements !! 1

parseLogFile :: C.ByteString -> [LogEntry]
parseLogFile content = map parseLogLine $ C.lines content

