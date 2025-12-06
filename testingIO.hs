import Prelude hiding (product)
import System.IO  -- first load modules
import Data.Char (isDigit, toLower)
import Text.Read (readMaybe)

data Category = Electronics | Books | Clothing | Groceries deriving (Show, Eq, Read)

data Product = Product {pid :: Int, pname :: String, price :: Float, category :: Category}
instance Show Product where
    show (Product id name price category) = "Product id: " ++ show id ++ ", product: " ++ show name ++ ", price: " ++ show price ++ ", category: " ++ show category
instance Eq Product where
        (Product pid1 _ _ _) == (Product pid2 _ _ _) = pid1 == pid2 --add on from 5 - needed in 5

data LoyaltyLevel = Bronze | Silver | Gold deriving (Show, Eq, Read)

data Customer = Customer {cid :: Int, cname :: String, loyaltyLevel :: LoyaltyLevel}

instance Show Customer where
    show (Customer id name loyaltyLevel) = "Customer id: " ++ show id ++ ", customer: " ++ show name ++ ", loyalty level: " ++ show loyaltyLevel
instance Eq Customer where
  (Customer id1 _ _) == (Customer id2 _ _) = id1 == id2

data CartItem = CartItem {product :: Product, quantity :: Int}

instance Show CartItem where
    show (CartItem product quantity) = "Item: " ++ show product ++ ", units: " ++ show quantity
instance Eq CartItem where
        (CartItem p1 q1) == (CartItem p2 q2) = p1 == p2 && q1 == q2

newtype ShoppingCart = ShoppingCart [CartItem]

instance Show ShoppingCart where
    show (ShoppingCart items) = "Shopping cart:\n" ++ unlines (map (\x -> "\t" ++ show x) items)

instance Eq ShoppingCart where
        (ShoppingCart items1) == (ShoppingCart items2) = items1 == items2

newtype Stock = Stock [(Product, Int)] -- product and their available quantity

instance Show Stock where
    show (Stock items) = "Stock:\n" ++ unlines (map (\(p, i) -> "\t" ++ show p ++ ", quantity available: " ++ show i) items)

data Status = Pending | Processing | Shipped | Delivered | Cancelled deriving (Show, Read, Eq)

data Order = Order {customer :: Customer, shoppingCart :: ShoppingCart, totalPrice :: Float, status :: Status}

instance Show Order where
    show (Order customer shoppingCart totalPrice status) = "Order for customer: " ++ show customer ++ "\n" ++ show shoppingCart 
        ++ "Total price: " ++ show totalPrice ++ ", status: " ++ show status ++ "\n"

instance Eq Order where
    (Order (Customer cid1 _ _) shpcrt1 _ _) == (Order (Customer cid2 _ _) shpcrt2 _ _) = cid1 == cid2 && shpcrt1 == shpcrt2

data SearchCriterion = ById Int | ByLoyaltyLevel LoyaltyLevel | ByProductId Int | ByCategory Category | ByTotalPrice Float deriving Show

class Discountable a where
  applyDiscount :: a -> Float -> Float

instance Discountable LoyaltyLevel where
  applyDiscount Bronze p = p
  applyDiscount Silver p = p * 0.95
  applyDiscount Gold   p = p * 0.90


instance Discountable Category where
  applyDiscount Books p = p * 0.85 -- Books 15% off
  applyDiscount _     p = p

newtype Error = Error [Product] deriving Show --created due to 7

-- 3
calculateProductPrice :: Product -> Float
calculateProductPrice (Product _ _ p _) = p

-- 4
calculateOrderTotal :: Order -> Float
calculateOrderTotal (Order cust (ShoppingCart items) _ _) =
  sum [ fromIntegral (quantity it) * priceAfter (product it) | it <- items ]
  where
    priceAfter prod = applyDiscount (loyaltyLevel cust)(applyDiscount (category prod)(calculateProductPrice prod))

-- 5
add :: CartItem -> ShoppingCart -> ShoppingCart
add item (ShoppingCart xs) = ShoppingCart (item:xs)

addToCart :: CartItem -> ShoppingCart -> ShoppingCart
addToCart item (ShoppingCart []) = ShoppingCart [item]
addToCart i@(CartItem product quantity) (ShoppingCart ((CartItem p q): cartList)) 
                | product == p = ShoppingCart (CartItem p (q+quantity) : cartList)
                | otherwise = add (CartItem p q) (addToCart i (ShoppingCart cartList))
--EXAMPLES
--product1 = Product 1 "Ipad" 12 Electronics
--product2 = Product 2 "Blouse"  15 Clothing
--product3 = Product 3 "Lemons"  5 Groceries
--productBook = Product 4 "Functional Programming for Dummies 101" 40 Books

--catalog :: [Product]
--catalog = [product1, product2, product3, productBook]

--stock = Stock (zip [product1, product2, product3, productBook] [2, 3, 4, 2])

--6
stockQuantity :: Stock -> Product -> Int
stockQuantity (Stock xs) p
  | null matches = 0
  | otherwise    = head matches
  where
    matches = [q | (r, q) <- xs, r == p]

checkStock :: Stock -> ShoppingCart -> [Product]
checkStock stock (ShoppingCart items) =
  [ p | CartItem p q <- items, stockQuantity stock p < q ]


--7
--createOrder :: Customer -> ShoppingCart -> Either Error Order
--createOrder customer c@(ShoppingCart xs) 
--        | null missing = Right (Order customer c totalPrice Pending)
--        | otherwise = Left (Error missing)
--        where 
--                missing = checkStock c
--                totalPrice = calculateOrderTotal (Order customer c 0 Pending)
                
createOrder :: Stock -> Customer -> ShoppingCart -> Either Error Order
createOrder stock customer c@(ShoppingCart xs)
  | null missing = Right (Order customer c totalPrice Pending)
  | otherwise    = Left (Error missing)
  where
    missing    = checkStock stock c
    totalPrice = calculateOrderTotal (Order customer c 0 Pending)

--8
transitionAllowed :: Status -> Status -> Bool
transitionAllowed Pending Processing = True
transitionAllowed Pending Cancelled  = True
transitionAllowed Processing Shipped    = True
transitionAllowed Processing Cancelled  = True
transitionAllowed Shipped Delivered  = True
transitionAllowed Shipped Cancelled  = True
transitionAllowed _ _ = False

updateOrderStatus :: Order -> Status -> Maybe Order
updateOrderStatus o@(Order _ _ _ old) new
  | transitionAllowed old new = Just (o { status = new })
  | otherwise             = Nothing

--9
isProductByIdInCart :: Int -> ShoppingCart -> Bool
isProductByIdInCart _ (ShoppingCart []) = False
isProductByIdInCart productId (ShoppingCart ((CartItem (Product pid _ _ _) _):xs)) 
        | productId == pid = True
        | otherwise = isProductByIdInCart productId (ShoppingCart xs)


isProductByCategoryInCart :: Category -> ShoppingCart -> Bool
isProductByCategoryInCart _ (ShoppingCart []) = False
isProductByCategoryInCart category (ShoppingCart ((CartItem (Product _ _ _ c) _):xs)) 
        | category == c = True
        | otherwise = isProductByCategoryInCart category (ShoppingCart xs)



searchOrders :: [SearchCriterion] -> [Order] -> [Order]
searchOrders [] orders = orders
searchOrders ((ById id): xs) orders = searchOrders xs (filter (\(Order (Customer cid _ _) _ _ _) -> cid == id) orders)
searchOrders ((ByLoyaltyLevel loyaltyLevel): xs) orders = searchOrders xs (filter (\(Order (Customer _ _ ll) _ _ _) -> loyaltyLevel == ll) orders)
searchOrders ((ByProductId id): xs) orders = searchOrders xs (filter (\(Order _ cart _ _) -> isProductByIdInCart id cart) orders)
searchOrders ((ByCategory category): xs) orders = searchOrders xs (filter (\(Order _ cart _ _) -> isProductByCategoryInCart category cart) orders)
searchOrders ((ByTotalPrice price): xs) orders = searchOrders xs (filter (\(Order _ _ p _) -> price == p) orders)

--10
-- helper 1
isActive :: Status -> Bool
isActive Delivered = False
isActive Cancelled = False
isActive _         = True

-- helper 2
activeOrders :: [Order] -> [Order]
activeOrders os =
  [ o | o@(Order _ _ _ st) <- os, isActive st ] --call to helper1
-- helper 3
customersOf :: [Order] -> [Customer]
customersOf os =
  [ c | Order c _ _ _ <- os ]

-- helper4 (no duplicates)
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs)
  | x `elem` xs = uniq xs
  | otherwise   = x : uniq xs

-- helper 5
totalOfCustomer :: Customer -> [Order] -> Float
totalOfCustomer cust os =
  sum [ t | Order c _ t _ <- os, c == cust ]

highValueCustomers :: [Order] -> Float -> [Customer]
highValueCustomers orders limit =
  [ c | c <- uniq (customersOf active), totalOfCustomer c active > limit] -- calls to helper 3 and helper 5
  where
    active = activeOrders orders --call ti helper 2
    
-- 11
splitOn :: Char -> String -> [String] -- (REUSED from 12 exactly the same)
splitOn _ "" = [""]
splitOn delimiter (x:xs)
    | x == delimiter = "" : rest
    | otherwise = (x : head rest) : tail rest
  where
    rest = splitOn delimiter xs

listToString :: Show a => [a] -> String -- (REUSED from 12 exactly the same)
listToString []   = ""
listToString (x:xs) = show x ++ "\n" ++ listToString xs

-- File I/O: Customers  (recycling Ivan's wonderful logic)
parseCustomer :: Customer -> String -- (REUSED from 12 exactly the same)
parseCustomer (Customer cid name ll) = show cid ++ "," ++ name ++ "," ++ show ll

saveCustomers :: Handle -> [Customer] -> IO () -- (REUSED from 12 exactly the same)
saveCustomers h = mapM_ (hPutStrLn h . parseCustomer)

parseCustomerLine :: String -> Customer -- (from 12 but rearranged logic...works exactly same)
parseCustomerLine line = Customer cid name ll
  where
    [cidStr, name, llStr] = splitOn ',' line
    cid = read cidStr
    ll  = read llStr
{- 
parseCustomerLine :: String -> Customer
parseCustomerLine line = Customer id name loyaltyLevel
  where
    [idStr, name, loyaltyLevelStr] = splitOn ',' line
    id = read idStr :: Int
    loyaltyLevel = read loyaltyLevelStr :: LoyaltyLevel
-}
readCustomer :: FilePath -> IO [Customer] -- NEW LOGIC -- Force full file read to avoid Windows file-lock from lazy readFile.
readCustomer fp = do
  contents <- readFile fp
  forceCustomers (map parseCustomerLine (lines contents))
  where
    forceCustomers :: [Customer] -> IO [Customer]
    forceCustomers []     = return []
    forceCustomers (c:cs) = do
      c `seq` forceCustomers cs
      return (c:cs)

-- File I/O: Stock  (using Ivan's logic)
parseStock :: Stock -> String -- (REUSED from 12 exactly the same)
parseStock (Stock items) =  unlines [ show pid ++ "," ++ name ++ "," ++ show price ++ "," ++ show cat ++ "," ++ show qty | (Product pid name price cat, qty) <- items ]

saveStock :: Handle -> Stock -> IO () -- (REUSED from 12 exactly the same)
saveStock h stock = hPutStrLn h (parseStock stock)

parseProductQuantityLine :: String -> (Product, Int) -- (from 12 but rearranged logic...works exactly same)
parseProductQuantityLine line = (Product pid name price cat, qty)
  where
    [pidStr, name, priceStr, catStr, qtyStr] = splitOn ',' line
    pid   = read pidStr
    price = read priceStr
    cat   = read catStr
    qty   = read qtyStr
{-
parseProductQuantityLine :: String -> (Product, Int)
parseProductQuantityLine line = (Product id name price category, qty)
  where
    [idStr, name, priceStr, categoryStr, qtyStr] = splitOn ',' line
    id = read idStr :: Int
    price = read priceStr :: Float
    category = read categoryStr :: Category
    qty = read qtyStr :: Int
-}
readStock :: FilePath -> IO Stock -- NEW LOGIC -- Force full file read to avoid Windows file-lock from lazy readFile.
readStock fp = do
  contents <- readFile fp
  forceList (parseAll (lines contents))
  where
    parseAll :: [String] -> [(Product, Int)]
    parseAll [] = []
    parseAll (x:xs)
      | null x    = parseAll xs
      | otherwise = parseProductQuantityLine x : parseAll xs

    forceList :: [(Product, Int)] -> IO Stock
    forceList [] = return (Stock [])
    forceList ((p,q):rest) = do
      p `seq` q `seq` forceList rest
      return (Stock ((p,q):rest))

-- File I/O: Orders  (Ivan's) -- (REUSED from 12 exactly the same)
parsePriceStatusLine :: String -> (Float, Status) -- (from 12 but rearranged logic...works exactly same)
parsePriceStatusLine line = (totalPrice, status)
  where
    [priceStr, statusStr] = splitOn ',' (drop 6 line)
    totalPrice = read priceStr
    status     = read statusStr
{-
parsePriceStatusLine :: String -> (Float, Status)
parsePriceStatusLine line = (totalPrice, status)
  where
    [priceStr, statusStr] = splitOn ',' (drop 6 line)  -- Remove "Price " prefix
    totalPrice = read priceStr :: Float
    status = read statusStr :: Status
-}
readCartItems :: [String] -> ([CartItem], [String]) -- (REUSED from 12 exactly the same)
readCartItems [] = ([], [])
readCartItems (line:rest)
  | take 5 line == "Price" = ([], line:rest)
  | otherwise              = (CartItem product qty : items, remaining)
  where
    (items, remaining) = readCartItems rest
    (product, qty)     = parseProductQuantityLine line

readOrderFromLines :: [String] -> (Order, [String]) -- (REUSED from 12 exactly the same)
readOrderFromLines [] = error "No order data"
readOrderFromLines (customerLine:rest) = (order, remaining)
  where
    customer = parseCustomerLine (drop 9 customerLine)
    (cartItems, priceAndRest) = readCartItems rest
    (totalPrice, status) = parsePriceStatusLine (head priceAndRest)
    shoppingCart = ShoppingCart cartItems
    order = Order customer shoppingCart totalPrice status
    remaining  = tail priceAndRest

-- Read all orders from file
readAllOrdersFromLines :: [String] -> [Order] -- (REUSED from 12 exactly the same)
readAllOrdersFromLines [] = []
readAllOrdersFromLines lines = order : readAllOrdersFromLines remaining
  where
    (order, remaining) = readOrderFromLines lines

-- Read orders from file
readOrdersFromFile :: FilePath -> IO [Order] -- NEW LOGIC -- Force full file read to avoid Windows file-lock from lazy readFile.
readOrdersFromFile filepath = do
  contents <- readFile filepath
  forceList (readAllOrdersFromLines (lines contents))
  where -- strictly evaluate head, then recurse to force the entire list
    forceList :: [Order] -> IO [Order]
    forceList []     = return []
    forceList (o:os) = do
      o `seq` forceList os
      return (o:os)

-- Writing orders
formatCartItem :: CartItem -> String -- (REUSED from 12 exactly the same)
formatCartItem (CartItem (Product pid name price cat) qty) =
  show pid ++ "," ++ name ++ "," ++ show price ++ "," ++ show cat ++ "," ++ show qty

formatPriceStatus :: Float -> Status -> String -- (REUSED from 12 exactly the same)
formatPriceStatus totalPrice status =
  "Price " ++ show totalPrice ++ "," ++ show status

writeOrderToHandle :: Handle -> Order -> IO () -- (REUSED from 12 exactly the same)
writeOrderToHandle h (Order cust (ShoppingCart items) totalPrice status) = do
  hPutStrLn h ("Customer " ++ parseCustomer cust)
  mapM_ (hPutStrLn h . formatCartItem) items
  hPutStrLn h (formatPriceStatus totalPrice status)

writeOrdersToFile :: FilePath -> [Order] -> IO () -- (REUSED from 12 exactly the same)
writeOrdersToFile fp orders =
  withFile fp WriteMode $ \h ->
    mapM_ (writeOrderToHandle h) orders

-- each user needs to identify himself/herself --helper 1 - search by name
searchProductsByName :: String -> [Product] -> [Product]
searchProductsByName name ps =
  [ p | p@(Product _ n _ _) <- ps, n == name ]
  
--helper 2- search by category
searchProductsByCategory :: Category -> [Product] -> [Product]
searchProductsByCategory cat ps =
  [ p | p@(Product _ _ _ c) <- ps, c == cat ]

-- helper 3 - search by max price
searchProductsByMaxPrice :: Float -> [Product] -> [Product]
searchProductsByMaxPrice maxP ps =
  [ prod | prod@(Product _ _ price _) <- ps, price <= maxP ]
  
-- helper 4- prod by id
findProductById :: Int -> [Product] -> Maybe Product
findProductById _ [] = Nothing
findProductById pid (p@(Product pid' _ _ _) : ps)
  | pid == pid' = Just p
  | otherwise   = findProductById pid ps

-- helper 5 - read categ from user
readCategory :: String -> Maybe Category
readCategory "Electronics" = Just Electronics
readCategory "Books"       = Just Books
readCategory "Clothing"    = Just Clothing
readCategory "Groceries"   = Just Groceries
readCategory _             = Nothing

-- helper 6 - printer
printProducts :: [Product] -> IO ()
printProducts [] = putStrLn "No products found."
printProducts [p] = print p
printProducts (p:ps) = do
  print p
  printProducts ps

--------------------------------------------------------------------------------
-- Product search I/O (derive catalog from Stock)
--------------------------------------------------------------------------------
catalogFromStock :: Stock -> [Product]
catalogFromStock (Stock items) = [ p | (p, qty) <- items, qty > 0 ]

searchProductIO :: Stock -> IO ()
searchProductIO stock = do
  putStrLn "Enter product name:"
  name <- getLine
  printProducts (searchProductsByName name (catalogFromStock stock))

searchCategoryIO :: Stock -> IO ()
searchCategoryIO stock =
  do putStrLn "Enter category (Electronics, Books, Clothing, Groceries):"
     s <- getLine
     handle (readCategory s)
  where
    handle Nothing = putStrLn "Unknown category."
    handle (Just cat) = printProducts (searchProductsByCategory cat (catalogFromStock stock))

searchMaxPriceIO :: Stock -> IO ()
searchMaxPriceIO stock =
  do putStrLn "Enter maximum price:"
     s <- getLine
     handle (readMaybe s :: Maybe Float)
  where
    handle :: Maybe Float -> IO ()
    handle Nothing = putStrLn "That is not a number."
    handle (Just mp) =  printProducts (searchProductsByMaxPrice mp (catalogFromStock stock))

--------------------------------------------------------------------------------
-- Add to cart I/O -- use addToCart from 5
--------------------------------------------------------------------------------
addToCartIO :: Stock -> ShoppingCart -> IO ShoppingCart
addToCartIO stock cart =
  do putStrLn "Enter product ID:"
     sId  <- getLine
     putStrLn "Enter quantity:"
     sQty <- getLine
     processInputs (readMaybe sId :: Maybe Int) (readMaybe sQty :: Maybe Int)
  where
    processInputs :: Maybe Int -> Maybe Int -> IO ShoppingCart
    processInputs (Just pid) (Just qty) =
      addToCartHelper pid qty (catalogFromStock stock) cart

    processInputs _ _ =
      do putStrLn "Invalid ID or quantity."
         return cart

addToCartHelper :: Int -> Int -> [Product] -> ShoppingCart -> IO ShoppingCart
addToCartHelper pid qty catalog cart = addToCartMaybe (findProductById pid catalog) qty cart

addToCartMaybe :: Maybe Product -> Int -> ShoppingCart -> IO ShoppingCart
addToCartMaybe Nothing _ cart =
  do putStrLn "Product not found."
     return cart

addToCartMaybe (Just p) qty cart =
  do putStrLn "Product added to cart."
     return (addToCart (CartItem p qty) cart)

--------------------------------------------------------------------------------
-- Customer identification (new vs existing)
--------------------------------------------------------------------------------
findCustomerById :: Int -> [Customer] -> Maybe Customer
findCustomerById _ [] = Nothing
findCustomerById cid (c@(Customer cid' _ _) : cs)
  | cid == cid' = Just c
  | otherwise   = findCustomerById cid cs

readLoyaltyLevel :: String -> Maybe LoyaltyLevel
readLoyaltyLevel s
  | ls == "bronze" = Just Bronze
  | ls == "silver" = Just Silver
  | ls == "gold"   = Just Gold
  | otherwise      = Nothing
  where
    ls = map toLower s

nextCustomerId :: [Customer] -> Int
nextCustomerId [] = 1
nextCustomerId cs = 1 + maximum [cid | Customer cid _ _ <- cs]

identifyCustomer :: [Customer] -> IO (Customer, [Customer])
identifyCustomer customers = do
  putStrLn "Are you an existing customer (1) or a new customer (2)? (b to exit)"
  ans <- getLine
  handleAnswer ans customers
  where
    handleAnswer :: String -> [Customer] -> IO (Customer, [Customer])
    handleAnswer "1" cs = existingCustomerFlow cs
    handleAnswer "2" cs = newCustomerFlow cs
    handleAnswer "b" cs = do
      putStrLn "Goodbye."
      fail "User aborted"
    handleAnswer _ cs = do
      putStrLn "Unknown option"
      identifyCustomer cs

existingCustomerFlow :: [Customer] -> IO (Customer, [Customer])
existingCustomerFlow customers = do
  putStrLn "Please enter your customer ID:"
  sId <- getLine
  handleIdParse (readMaybe sId :: Maybe Int) customers
  where
    handleIdParse :: Maybe Int -> [Customer] -> IO (Customer, [Customer])
    handleIdParse Nothing cs = do
      putStrLn "ID must be an integer."
      existingCustomerFlow cs
    handleIdParse (Just cid) cs =
      handleCustomerLookup (findCustomerById cid cs) cs

    handleCustomerLookup :: Maybe Customer -> [Customer] -> IO (Customer, [Customer])
    handleCustomerLookup Nothing cs = do
      putStrLn "No customer with that ID."
      existingCustomerFlow cs
    handleCustomerLookup (Just c) cs = do
      putStrLn ("Welcome back, " ++ cname c ++ "!")
      return (c, cs)

newCustomerFlow :: [Customer] -> IO (Customer, [Customer])
newCustomerFlow customers = do
  putStrLn "Registering a new customer."
  putStrLn "Enter your name:"
  name <- getLine
  success customers name Bronze

success :: [Customer] -> String -> LoyaltyLevel -> IO (Customer, [Customer])
success customers name ll = do
  putStrLn ("Your new customer ID is " ++ show cid)
  return (newC, customers ++ [newC])
  where
    cid  = nextCustomerId customers
    newC = Customer cid name ll

updateStockWithCart :: Stock -> ShoppingCart -> Stock
updateStockWithCart (Stock items) (ShoppingCart cartItems) =
  Stock (foldl updateOne items cartItems)
  where
    updateOne :: [(Product, Int)] -> CartItem -> [(Product, Int)]
    updateOne acc (CartItem (Product pid _ _ _) q) =
      [ if pid' == pid then (p, qty - q) else (p, qty) | (p@(Product pid' _ _ _), qty) <- acc ]

writeCustomersToFile :: FilePath -> [Customer] -> IO ()
writeCustomersToFile filepath customers =
  withFile filepath WriteMode $ \handle ->
    saveCustomers handle customers

writeStockToFile :: FilePath -> Stock -> IO ()
writeStockToFile filepath stock =
  withFile filepath WriteMode $ \handle ->
    saveStock handle stock

shopLoop :: Stock -> ShoppingCart -> IO (ShoppingCart, Stock)
shopLoop stock cart = do
  putStrLn "\nWhat would you like to do?"
  putStrLn "1. Search products by name"
  putStrLn "2. Search products by category"
  putStrLn "3. Search products by maximum price"
  putStrLn "4. Add product to cart"
  putStrLn "5. View cart"
  putStrLn "6. Checkout"
  putStrLn "b. Exit without ordering"
  choice <- getLine
  handle choice stock cart
  where
    handle :: String -> Stock -> ShoppingCart -> IO (ShoppingCart, Stock)

    handle "1" st ct = do
      searchProductIO st
      shopLoop st ct
    handle "2" st ct = do
      searchCategoryIO st
      shopLoop st ct
    handle "3" st ct = do
      searchMaxPriceIO st
      shopLoop st ct
    handle "4" st ct = do
      ct' <- addToCartIO st ct
      shopLoop st ct'
    handle "5" st ct = do
      putStrLn "Your cart:"
      print ct
      shopLoop st ct
    handle "6" st ct =
      return (ct, st)
    handle "b" st ct =
      return (ct, st)
    handle _ st ct = do
      putStrLn "Unknown option."
      shopLoop st ct
--------------------------------------------------------------------------------
-- Place order using createOrder (from 7) and update files
--------------------------------------------------------------------------------
placeOrderIO :: Stock -> Customer -> ShoppingCart -> IO (Either Error Order)
placeOrderIO stock cust cart =
  handleCreate (createOrder stock cust cart)
  where
    handleCreate (Left err) = do
      putStrLn ("Order could not be created: " ++ show err)
      return (Left err)

    handleCreate (Right order) = do
      putStrLn "Order created successfully:"
      print order
      return (Right order)

customerSession :: IO ()
customerSession = do
  putStrLn "Welcome to Ivan's and Isa's shop!"

  customers <- readCustomer "customers.txt"
  stock     <- readStock    "stock.txt"
  orders    <- readOrdersFromFile "orders.txt"

  (customer, customers') <- identifyCustomer customers

  cartHandler (shopLoop stock (ShoppingCart [])) customer customers' stock orders
  where
    cartHandler action cust customers' st orders = do
      (finalCart, stockBeforeOrder) <- action
      handleCart finalCart cust customers' stockBeforeOrder orders

    handleCart (ShoppingCart []) _ customers' stockBeforeOrder orders = do
      putStrLn "Your cart is empty. Nothing to do."
      writeCustomersToFile "customers.txt" customers'
      writeStockToFile     "stock.txt"     stockBeforeOrder
      writeOrdersToFile    "orders.txt"    orders
      putStrLn "Goodbye."

    handleCart cart cust customers' stockBeforeOrder orders = do
      handleOrder (placeOrderIO stockBeforeOrder cust cart)
                  cart cust customers' stockBeforeOrder orders

    handleOrder action cart cust customers' stockBeforeOrder orders = do
      result <- action
      orderResult result cart cust customers' stockBeforeOrder orders

    orderResult (Left _) _ _ customers' stockBeforeOrder orders = do
      putStrLn "Order failed. No changes saved (except new customers)."
      writeCustomersToFile "customers.txt" customers'
      writeStockToFile     "stock.txt"     stockBeforeOrder
      writeOrdersToFile    "orders.txt"    orders

    orderResult (Right newOrder) cart _ customers' stockBeforeOrder orders = do
      putStrLn "Updating files..."
      writeCustomersToFile "customers.txt" customers'
      writeStockToFile     "stock.txt"     (updateStockWithCart stockBeforeOrder cart)
      writeOrdersToFile    "orders.txt"    (orders ++ [newOrder])
      putStrLn "Order placed and data saved. Thank you!"

main :: IO ()
main = customerSession
