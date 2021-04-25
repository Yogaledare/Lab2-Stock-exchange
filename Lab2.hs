import Control.Applicative
import System.Environment
import System.IO

import Data.List (intercalate)


import SkewHeap

-- | Bids.

data Bid
    = Buy Person Price           -- Person offers to buy share
    | Sell Person Price          -- Person offers to sell share
    | NewBuy Person Price Price  -- Person changes buy bid
    | NewSell Person Price Price -- Person changes sell bid
    deriving Show

type Person = String
type Price = Integer


data BuyBid = BuyBid Person Price deriving Eq

-- instance Eq BuyBid where
--     (BuyBid _ price1) == (BuyBid _ price2) = price1 == price2

instance Ord BuyBid where
    compare (BuyBid _ price1) (BuyBid _ price2) = compare price2 price1

instance Show BuyBid where
    show (BuyBid name price) = show (name ++ " " ++ show price)


data SellBid = SellBid Person Price deriving Eq

-- instance Eq SellBid where
--     (SellBid _ price1) == (SellBid _ price2) = price1 == price2

instance Ord SellBid where
    compare (SellBid _ price1) (SellBid _ price2) = compare price1 price2

instance Show SellBid where
    show (SellBid name price) = show (name ++ " " ++ show price)



-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

testRead :: String -> Maybe Integer
testRead s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing


parseBid :: String -> Either String Bid
parseBid s = case words s of
    name : kind : prices ->
        case (kind, mapM readInteger prices) of
        ("K",  Just [price])              -> Right (Buy name price)
        ("S",  Just [price])              -> Right (Sell name price)
        ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
        ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
        _ -> Left s
    _ -> Left s

testa1 = parseBid "K Ada 50"
testa2 = words "K Ada 50"

readInteger :: String -> Maybe Integer
readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
    where
        check (Left bid)  = do
            hPutStrLn stderr $ "Malformed bid: " ++ bid
            return []
        check (Right bid) = return [bid]

-- | The main function of the program.

main :: IO ()
main = do
    args <- getArgs
    case args of
        []  -> process stdin
        [f] -> process =<< openFile f ReadMode
        _   -> hPutStr stderr $ unlines
            [ "Usage: ./Lab2 [<file>]"
                , "If no file is given, then input is read from standard input."
            ]
    where
        process :: Handle -> IO ()
        process h = trade =<< parseBids =<< hGetContents h


-- | The core of the program. Takes a list of bids and executes them.
data OrderBook = OrderBook (SkewHeap BuyBid) (SkewHeap SellBid) deriving Show


trade :: [Bid] -> IO ()
trade bids = do
    let emptyOrderBook = OrderBook Empty Empty
    manageOrderBook emptyOrderBook bids
    return ()


toCommaSeparatedString :: [String] -> String
toCommaSeparatedString = intercalate ", "


manageOrderBook :: OrderBook -> [Bid] -> IO ()
manageOrderBook (OrderBook buyBids sellBids) [] = 
    do
        putStrLn "\nOrder book:"
        let sortedSellBidsStrings = map (filter (/= '"') . show) $ sorted sellBids
        let sortedBuyBidsStrings  = map (filter (/= '"') . show) $ sorted buyBids
        putStrLn ("Sellers: " ++ toCommaSeparatedString sortedSellBidsStrings)
        putStrLn ("Buyers: " ++ toCommaSeparatedString sortedBuyBidsStrings)
        return ()
manageOrderBook oldOrderBook@(OrderBook buyBids sellBids) (bid:bids) = 
    do
        -- print (bid:bids)
        let updatedBook = addToOrderBook oldOrderBook bid
        -- print updatedBook
        let clearedBook = tryClearTopBids updatedBook
        -- print clearedBook
        case clearedBook of
            Left (buyer, seller, price, newOrderBook) -> 
                do
                    putStrLn (buyer ++ " buys a share from " ++ 
                        seller ++ " for " ++ show price ++ "kr")
                    manageOrderBook newOrderBook bids
                    return ()
            Right unchangedOrderBook ->
                do
                    manageOrderBook unchangedOrderBook bids
                    return ()


addToOrderBook :: OrderBook -> Bid -> OrderBook
addToOrderBook (OrderBook buyBids sellBids) bid = case bid of 
    (Buy name price)       -> OrderBook
                                  (insert (BuyBid name price) buyBids)
                                  sellBids
    (Sell name price)      -> OrderBook
                                  buyBids
                                  (insert (SellBid name price) sellBids)
    (NewBuy name 
        oldPrice newPrice) -> OrderBook
                                  (insert (BuyBid name newPrice)
                                      $ delete (BuyBid name oldPrice) buyBids)
                                  sellBids
    (NewSell name 
        oldPrice newPrice) -> OrderBook
                                  buyBids 
                                  (insert (SellBid name newPrice) $ 
                                      delete (SellBid name oldPrice) sellBids)




tryClearTopBids :: OrderBook -> Either (Person, Person, Price, OrderBook) OrderBook
tryClearTopBids oldOrderBook@(OrderBook buyBids sellBids) = 
    let buyRoot  = extractRoot buyBids
        sellRoot = extractRoot sellBids in
    case (buyRoot, sellRoot) of
        (Just (BuyBid buyName highestBuy, updatedBuyBids), Just (SellBid sellName lowestSell, updatedSellBids))
            | highestBuy >= lowestSell -> Left (buyName, sellName, highestBuy, newOrderBook)
            where
                newOrderBook = OrderBook updatedBuyBids updatedSellBids
        _ -> Right oldOrderBook










test17 = OrderBook Empty Empty
test15 = [Buy "Ada" 54, Buy "Kålle" 56]
test16 = Buy "Ada" 12
test x = 5 + x
ada = BuyBid "Ada" 50
kålle = BuyBid "Kålle" 50
abel = BuyBid "Abel" 53
kain = BuyBid "Kain" 43
sven = BuyBid "Sven" 59
anna = BuyBid "Anna" 73
marcus = BuyBid "Marcus" 72
heap1 = Node ada Empty heap2 
heap2 = Node kålle Empty Empty 
heap3 = Node abel Empty Empty  
heap4 = Node kain Empty heap1
test3 = Node sven Empty Empty 
test6 = merge heap4 test3
test7 = merge test6 (Node anna (Node marcus Empty Empty) Empty)
test8 = insert ada heap2
test9 = insert kain test8
test10 = insert abel test9
test11 = insert marcus $ insert anna $ insert sven test10
test12 = extractRoot test11
testc1 = concatMap (\item -> show item ++ ", ") ["a", "b", "c"]
testc2 = putStrLn testc1





-- tryClearTopBids :: OrderBook -> Either (Person, Person, Price, OrderBook) OrderBook
-- tryClearTopBids oldOrderBook@(OrderBook buyBids sellBids) =
--     if  extractRoot buyBids == Just (BuyBid buyName highestBuy, updatedBuyBids) &&
--             extractRoot sellBids == Just (SellBid sellName lowestSell, updatedSellBids)
--         then if highestBuy >= lowestSell
--             then let 
--                 newOrderBook = OrderBook updatedBuyBids updatedSellBids in
--                     Left (buyName, sellName, highestBuy, newOrderBook)
--         else Right oldOrderBook


--     | highestBuy >= lowestSell = clearedResponse
--     | otherwise                = notClearedResponse
--     where
--         (buyRoot, sellRoot) = (extractRoot buyBids, extractRoot sellBids)
--         if ()
        

--  Right oldOrderBook
-- Left (buyName, sellName, highestBuy, newOrderBook)


    -- let buyRoot  = extractRoot buyBids
    --     sellRoot = extractRoot sellBids in
    -- case (buyRoot, sellRoot) of
    --     (Just (BuyBid buyName highestBuy, updatedBuyBids), Just (SellBid sellName lowestSell, updatedSellBids))
    --         | highestBuy >= lowestSell -> Left (buyName, sellName, highestBuy, newOrderBook)
    --         where
    --             newOrderBook = OrderBook updatedBuyBids updatedSellBids
    --     _ -> Right oldOrderBook



--     | highestBuy >= lowestSell = Left (buyName, sellName, highestBuy, newOrderBook)
--     | otherwise = Right oldOrderBook
--     where
--         highestBuy = 



--     case (extractRoot buyBids, extractRoot sellBids) of 
--         let a = Just (BuyBid buyName highestBuy, updatedBuyBids)
--         let b = Just (SellBid sellName lowestSell, updatedSellBids)) in 
--             (a, b)
        
--             | highestBuy >= lowestSell -> Left (buyName, sellName, highestBuy, newOrderBook)
--                 where
--                     newOrderBook = OrderBook updatedBuyBids updatedSellBids
--         _ -> Right oldOrderBook

-- (Just (BuyBid buyName highestBuy, updatedBuyBids), Just (SellBid sellName lowestSell, updatedSellBids))

                -- putStrLn (buyName ++ " buys a share from " ++ 
                --     sellName ++ " for " ++ show highestBuy ++ "kr")




-- trade [] = return "done"
-- trade (bid:bids) = do

    -- foldl (tryClearTopBids . addToOrderBook) finishedBook bids
    -- let startOrderBook = OrderBook Empty Empty
    -- let finishedOrderBook = foldl addToOrderBook startOrderBook bids
    -- finishedbook <- foldl (addToOrderBook) startOrderBook bids
    -- reducedOrderBook <- [tryClearTopBids entry | entry <- finishedOrderBook]
    -- finishedOrderBook <- foldl addToOrderBook startOrderBook bids
        -- startorderbook bids
        -- [addToOrderBook bid startorderbook | bid <- bids]
    -- orderbook <-  
    -- putStrLn "hej"
    -- if (bids != []) return trade bids 
    -- else return sorted startOrderBook
    -- [(tryClearTopBids . addToOrderBook) bid | bid <- bids]
    -- return putStrLn show finishedBook
     


-- tryClearTopBids :: OrderBook -> IO OrderBook
-- tryClearTopBids oldOrderBook@(OrderBook buyBids sellBids) = 
--     case (extractRoot buyBids, extractRoot sellBids) of 
--         (Just (BuyBid buyName highestBuy, updatedBuyBids), 
--             Just (SellBid sellName lowestSell, updatedSellBids))
--             | highestBuy >= lowestSell -> do
--                 putStrLn (buyName ++ " buys a share from " ++ 
--                     sellName ++ " for " ++ show highestBuy ++ "kr")
--                 return (OrderBook updatedBuyBids updatedSellBids)
--         _ -> return oldOrderBook




-- tryClearTopBids :: OrderBook -> OrderBook
-- tryClearTopBids (OrderBook buyBids sellBids) 
--     case (p1@(peek buyBids), p2@(peek sellBids)) of
--         (Just buyPrice, Just sellPrice)
--             | buyPrice >= sellPrice -> 


--     | peek buyBids >= peek sellBids = 





-- test18 = addToOrderBook test16 $ addToOrderBook test16 test17


-- addToOrderBook (Buy name price) (OrderBook buyBook sellBook) = 
--     OrderBook (insert (BuyBid name price) buyBook) sellBook
-- addToOrderBook (Sell name price) (OrderBook buyBook sellBook) =
--     OrderBook buyBook (insert (SellBid name price) sellBook)
-- addToOrderBook (NewBuy name oldPrice newPrice) (OrderBook buyBook sellBook) =
--     OrderBook (insert (BuyBid name newPrice) $ delete (BuyBid name oldPrice) buyBook) sellBook
-- addToOrderBook (NewSell name oldPrice newPrice) (OrderBook buyBook sellBook) = 
--     OrderBook buyBook (insert (SellBid name newPrice) $ delete (SellBid name oldPrice) sellBook)



-- addToOrderBook :: Bid -> OrderBook -> OrderBook
-- addToOrderBook bid (OrderBook buyBook sellBook) = OrderBook newBuyBook newSellBook
--     where
--         newBuyBook = case bid of
--             (Buy name price) -> insert (BuyBid name price) buyBook
--             (NewBuy name oldPrice newPrice) -> insert (BuyBid name newPrice) $
--                 delete (BuyBid name oldPrice) buyBook
--             _ -> buyBook
--         newSellBook = case bid of 
--             (Sell name price) -> insert (SellBid name price) sellBook
--             (NewSell name oldPrice newPrice) -> insert (SellBid name newPrice) $ 
--                 delete (SellBid name oldPrice) sellBook
--             _ -> sellBook

    
    
    
    -- case bid of 
    -- (Buy name price)        ->  OrderBook
    --                                 (insert (BuyBid name price) buyBook)
    --                                 sellBook
    -- (Sell name price)       ->  OrderBook
    --                                 buyBook
    --                                 (insert (SellBid name price) sellBook)
    -- (NewBuy name 
    --     oldPrice newPrice)  ->  OrderBook
    --                                 (insert (BuyBid name newPrice)
    --                                     $ delete (BuyBid name oldPrice) buyBook)
    --                                 sellBook
    -- (NewSell name 
    --     oldPrice newPrice)  ->  OrderBook
    --                                 buyBook 
    --                                 (insert (SellBid name newPrice) $ 
    --                                     delete (SellBid name oldPrice) sellBook)



