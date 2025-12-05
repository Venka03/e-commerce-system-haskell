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
-- (REUSED from 12 slightly improved :)
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delimiter (x:xs)
    | x == delimiter = "" : rest
    | otherwise      = (x : head rest) : tail rest
  where
    rest = splitOn delimiter xs

listToString :: Show a => [a] -> String
listToString []     = ""
listToString (x:xs) = show x ++ "\n" ++ listToString xs
--------------------------------------------------------------------------------
-- File I/O: Customers  (recycling Ivan's wonderful logic)
--------------------------------------------------------------------------------
parseCustomer :: Customer -> String
parseCustomer (Customer cid name ll) =
  show cid ++ "," ++ name ++ "," ++ show ll

saveCustomers :: Handle -> [Customer] -> IO ()
saveCustomers h = mapM_ (hPutStrLn h . parseCustomer)

parseCustomerLine :: String -> Customer
parseCustomerLine line = Customer cid name ll
  where
    [cidStr, name, llStr] = splitOn ',' line
    cid = read cidStr
    ll  = read llStr

readCustomer :: FilePath -> IO [Customer]
readCustomer fp = do
  contents <- readFile fp
  return (map parseCustomerLine (lines contents))
--------------------------------------------------------------------------------
-- File I/O: Stock  (using Ivan's logic)
--------------------------------------------------------------------------------
parseStock :: Stock -> String
parseStock (Stock items) =
  unlines
    [ show pid ++ "," ++ name ++ "," ++ show price ++ "," ++ show cat ++ "," ++ show qty | (Product pid name price cat, qty) <- items ]

saveStock :: Handle -> Stock -> IO ()
saveStock h stock = hPutStrLn h (parseStock stock)

parseProductQuantityLine :: String -> (Product, Int)
parseProductQuantityLine line = (Product pid name price cat, qty)
  where
    [pidStr, name, priceStr, catStr, qtyStr] = splitOn ',' line
    pid   = read pidStr
    price = read priceStr
    cat   = read catStr
    qty   = read qtyStr

readStock :: FilePath -> IO Stock
readStock fp = do
  contents <- readFile fp
  let stockItems = map parseProductQuantityLine (filter (not . null) (lines contents))
  return (Stock stockItems)
--------------------------------------------------------------------------------
-- File I/O: Orders  (Ivan's)
--------------------------------------------------------------------------------
parsePriceStatusLine :: String -> (Float, Status)
parsePriceStatusLine line = (totalPrice, status)
  where
    [priceStr, statusStr] = splitOn ',' (drop 6 line)
    totalPrice = read priceStr
    status     = read statusStr

readCartItems :: [String] -> ([CartItem], [String]) -- Read cart item lines until "Price"
readCartItems [] = ([], [])
readCartItems (line:rest)
  | take 5 line == "Price" = ([], line:rest)
  | otherwise              = (CartItem product qty : items, remaining)
  where
    (items, remaining) = readCartItems rest
    (product, qty)     = parseProductQuantityLine line

readOrderFromLines :: [String] -> (Order, [String])
readOrderFromLines [] = error "No order data"
readOrderFromLines (customerLine:rest) = (order, remaining)
  where
    customer = parseCustomerLine (drop 9 customerLine)  -- drop "Customer "
    (cartItems, priceAndRest) = readCartItems rest
    (totalPrice, status)      = parsePriceStatusLine (head priceAndRest)
    shoppingCart              = ShoppingCart cartItems
    order                     = Order customer shoppingCart totalPrice status
    remaining                 = tail priceAndRest

readAllOrdersFromLines :: [String] -> [Order] --removed let/in
readAllOrdersFromLines [] = []
readAllOrdersFromLines ls =
  case readOrderFromLines ls of
    (order, remaining) -> order : readAllOrdersFromLines remaining

readOrdersFromFile :: FilePath -> IO [Order]
readOrdersFromFile fp = do
  contents <- readFile fp
  return (readAllOrdersFromLines (lines contents))

-- Writing orders
formatCartItem :: CartItem -> String
formatCartItem (CartItem (Product pid name price cat) qty) =
  show pid ++ "," ++ name ++ "," ++ show price ++ "," ++ show cat ++ "," ++ show qty

formatPriceStatus :: Float -> Status -> String
formatPriceStatus totalPrice status =
  "Price " ++ show totalPrice ++ "," ++ show status

writeOrderToHandle :: Handle -> Order -> IO ()
writeOrderToHandle h (Order cust (ShoppingCart items) totalPrice status) = do
  hPutStrLn h ("Customer " ++ parseCustomer cust)
  mapM_ (hPutStrLn h . formatCartItem) items
  hPutStrLn h (formatPriceStatus totalPrice status)

writeOrdersToFile :: FilePath -> [Order] -> IO ()
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
-- Product search I/O (use Stock -> derive catalog)
--------------------------------------------------------------------------------
catalogFromStock :: Stock -> [Product]
catalogFromStock (Stock items) = [ p | (p, qty) <- items, qty > 0 ]

searchProductIO :: Stock -> IO ()
searchProductIO stock = do
  let catalog = catalogFromStock stock
  putStrLn "Enter product name:"
  name <- getLine
  printProducts (searchProductsByName name catalog)

searchCategoryIO :: Stock -> IO ()
searchCategoryIO stock = do
  let catalog = catalogFromStock stock
  putStrLn "Enter category (Electronics, Books, Clothing, Groceries):"
  s <- getLine
  case readCategory s of
    Nothing  -> putStrLn "Unknown category."
    Just cat -> printProducts (searchProductsByCategory cat catalog)

searchMaxPriceIO :: Stock -> IO ()
searchMaxPriceIO stock = do
  let catalog = catalogFromStock stock
  putStrLn "Enter maximum price:"
  s <- getLine
  case readMaybe s :: Maybe Float of
    Nothing  -> putStrLn "That is not a number."
    Just mp  -> printProducts (searchProductsByMaxPrice mp catalog)

--------------------------------------------------------------------------------
-- Add to cart I/O (uses addToCart from 5)
--------------------------------------------------------------------------------
addToCartIO :: Stock -> ShoppingCart -> IO ShoppingCart
addToCartIO stock cart = do
  let catalog = catalogFromStock stock
  putStrLn "Enter product ID:"
  sId <- getLine
  putStrLn "Enter quantity:"
  sQty <- getLine
  case (readMaybe sId :: Maybe Int, readMaybe sQty :: Maybe Int) of
    (Just pid, Just qty) -> addToCartHelper pid qty catalog cart
    _   -> do
      putStrLn "Invalid ID or quantity."
      return cart

addToCartHelper :: Int -> Int -> [Product] -> ShoppingCart -> IO ShoppingCart
addToCartHelper pid qty catalog cart =
  addToCartMaybe (findProductById pid catalog) qty cart

addToCartMaybe :: Maybe Product -> Int -> ShoppingCart -> IO ShoppingCart
addToCartMaybe Nothing _ cart = do
  putStrLn "Product not found."
  return cart
addToCartMaybe (Just p) qty cart = do
  putStrLn "Product added to cart."
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
readLoyaltyLevel s =
  case map toLower s of
    "bronze" -> Just Bronze
    "silver" -> Just Silver
    "gold" -> Just Gold
    _  -> Nothing

nextCustomerId :: [Customer] -> Int
nextCustomerId [] = 1
nextCustomerId cs = 1 + maximum [cid | Customer cid _ _ <- cs]

identifyCustomer :: [Customer] -> IO (Customer, [Customer])
identifyCustomer customers = do
  putStrLn "Are you an existing customer (1) or a new customer (2)? (b to exit)"
  ans <- getLine
  case ans of
    "1" -> existingCustomerFlow customers
    "2" -> newCustomerFlow customers
    "b" -> do
      putStrLn "Goodbye."
      fail "User aborted"
    _   -> do
      putStrLn "I did not understand that."
      identifyCustomer customers

existingCustomerFlow :: [Customer] -> IO (Customer, [Customer])
existingCustomerFlow customers = do
  putStrLn "Please enter your customer ID:"
  sId <- getLine
  case readMaybe sId :: Maybe Int of
    Nothing -> do
      putStrLn "ID must be an integer."
      existingCustomerFlow customers
    Just cid ->
      case findCustomerById cid customers of
        Nothing -> do
          putStrLn "No customer with that ID."
          existingCustomerFlow customers
        Just c -> do
          putStrLn ("Welcome back, " ++ cname c ++ "!")
          return (c, customers)

newCustomerFlow :: [Customer] -> IO (Customer, [Customer])
newCustomerFlow customers = do
  putStrLn "Registering a new customer."
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn "Enter your loyalty level (Bronze, Silver, Gold):"
  llStr <- getLine
  maybe (retry customers) (success customers name) (readLoyaltyLevel llStr)

retry :: [Customer] -> IO (Customer, [Customer])
retry customers = do
  putStrLn "Unknown loyalty level."
  newCustomerFlow customers

success :: [Customer] -> String -> LoyaltyLevel -> IO (Customer, [Customer])
success customers name ll = do
  putStrLn ("Your new customer ID is " ++ show cid)
  return (newC, customers ++ [newC])
  where
    cid  = nextCustomerId customers
    newC = Customer cid name ll
--
updateStockWithCart :: Stock -> ShoppingCart -> Stock
updateStockWithCart (Stock items) (ShoppingCart cartItems) =
  Stock (foldl updateOne items cartItems)
  where
    updateOne :: [(Product, Int)] -> CartItem -> [(Product, Int)]
    updateOne acc (CartItem (Product pid _ _ _) q) =
      [ if pid' == pid then (p, qty - q) else (p, qty) | (p@(Product pid' _ _ _), qty) <- acc ]
--
shopLoop :: Stock -> ShoppingCart -> IO (ShoppingCart, Stock)
shopLoop stock cart = do
  let catalog = catalogFromStock stock
  putStrLn "\nWhat would you like to do?"
  putStrLn "1. Search products by name"
  putStrLn "2. Search products by category"
  putStrLn "3. Search products by maximum price"
  putStrLn "4. Add product to cart"
  putStrLn "5. View cart"
  putStrLn "6. Checkout"
  putStrLn "b. Exit without ordering"
  choice <- getLine
  case choice of
    "1" -> do
      searchProductIO stock
      shopLoop stock cart
    "2" -> do
      searchCategoryIO stock
      shopLoop stock cart
    "3" -> do
      searchMaxPriceIO stock
      shopLoop stock cart
    "4" -> do
      cart' <- addToCartIO stock cart
      shopLoop stock cart'
    "5" -> do
      putStrLn "Your cart:"
      print cart
      shopLoop stock cart
    "6" -> return (cart, stock)
    "b" -> return (cart, stock)
    _   -> do
      putStrLn "Unknown option."
      shopLoop stock cart
--
--------------------------------------------------------------------------------
-- Place order using createOrder (from 7) and update files
--------------------------------------------------------------------------------
placeOrderIO :: Stock -> Customer -> ShoppingCart -> IO (Either Error Order)
placeOrderIO stock cust cart =
  case createOrder stock cust cart of
    Left err -> do
      putStrLn ("Order could not be created: " ++ show err)
      return (Left err)
    Right order -> do
      putStrLn "Order created successfully:"
      print order
      return (Right order)

customerSession :: IO ()
customerSession = do
  putStrLn "Welcome to the shop!"

  customers <- readCustomer "customers.txt"
  stock     <- readStock    "stock.txt"
  orders    <- readOrdersFromFile "orders.txt"

  (customer, customers') <- identifyCustomer customers

  let emptyCart = ShoppingCart []

  (finalCart, stockBeforeOrder) <- shopLoop stock emptyCart

  case finalCart of
    ShoppingCart [] -> do
      putStrLn "Your cart is empty. Nothing to do."
      withFile "customers.txt" WriteMode $ \h -> saveCustomers h customers'
      withFile "stock.txt"     WriteMode $ \h -> saveStock h stockBeforeOrder
      writeOrdersToFile "orders.txt" orders
      putStrLn "Goodbye."
    _ -> do
      result <- placeOrderIO stockBeforeOrder customer finalCart
      case result of
        Left _ -> do
          putStrLn "Order failed. No changes saved (except new customers)."
          withFile "customers.txt" WriteMode $ \h -> saveCustomers h customers'
          withFile "stock.txt"     WriteMode $ \h -> saveStock h stockBeforeOrder
          writeOrdersToFile "orders.txt" orders
        Right newOrder -> do
          let newStock  = updateStockWithCart stockBeforeOrder finalCart
              newOrders = orders ++ [newOrder]
          putStrLn "Updating files..."
          withFile "customers.txt" WriteMode $ \h -> saveCustomers h customers'
          withFile "stock.txt"     WriteMode $ \h -> saveStock h newStock
          writeOrdersToFile "orders.txt" newOrders
          putStrLn "Order placed and data saved. Thank you!"

main :: IO ()
main = customerSession
