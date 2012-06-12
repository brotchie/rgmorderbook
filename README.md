# A Haskell solution to the RGM Advisors Order Book Programming Problem

RGM Advisors have a problem on their website (http://www.rgmadvisors.com/problems/orderbook/)
that requires you to reconstruct the order book for the Acme Internet Widget Company. 

This is an implementation in Haskell.

Notes:
 - The vanilla GHC String type is very slow---strings are stored as a linked-list of unicode characters. To get around this we use the Data.ByteString.Char8 data type.
