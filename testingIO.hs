import Prelude hiding (product)
import System.IO  -- first load modules
import Data.Char (isDigit, toLower)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

--

data Category = Electronics | Books | Clothing | Groceries deriving (Show, Eq, Read)

data Product = Product {pid :: Int, pname :: String, price :: Float, category :: Category}
instance Show Product where
    show (Product id name price category) = "Product id: " ++ show id ++ ", product: " ++ show name ++ ", price: " ++ printf "%.2f" price ++ ", category: " ++ show category

instance Eq Product where
    (Product pid1 _ _ _) == (Product pid2 _ _ _) = pid1 == pid2

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
    show (ShoppingCart items) = "Shopping cart:\n" ++ unlines (map (\x -> '\t': show x) items)

instance Eq ShoppingCart where
        (ShoppingCart items1) == (ShoppingCart items2) = items1 == items2

newtype Stock = Stock [(Product, Int)]

instance Show Stock where
    show (Stock items) = "Stock:\n" ++ unlines (map (\(p, i) -> "\t" ++ show p ++ ", quantity available: " ++ show i) items)

data Status = Pending | Processing | Shipped | Delivered | Cancelled deriving (Show, Read, Eq)

data Order = Order {customer :: Customer, shoppingCart :: ShoppingCart, totalPrice :: Float, status :: Status}

instance Show Order where
    show (Order customer shoppingCart totalPrice status) = "Order for customer: " ++ show customer ++ "\n" ++ show shoppingCart 
        ++ "Total price: " ++ printf "%.2f" totalPrice ++ ", status: " ++ show status ++ "\n"

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

newtype Error = Error [Product] --created due to 7
instance Show Error where
    show (Error products) = "Error: The following products are out of stock:\n" ++ unlines (map (\p -> '\t': show p) products)

calculateProductPrice :: Product -> Float
calculateProductPrice (Product _ _ p _) = p

myRound2 :: Float -> Float
myRound2 x = fromIntegral rounded / 100
  where
    scaled     = x * 100
    integer    = floor scaled
    fraction   = scaled - fromIntegral integer
    rounded
      | fraction >= 0.5 = integer + 1
      | otherwise       = integer
    
calculateOrderTotal :: Order -> Float
calculateOrderTotal (Order cust (ShoppingCart items) _ _) =
  myRound2 (sum [ fromIntegral (quantity it) * priceAfter (product it) | it <- items ])
  where
    priceAfter prod = applyDiscount (loyaltyLevel cust) (applyDiscount (category prod) (calculateProductPrice prod))

add :: CartItem -> ShoppingCart -> ShoppingCart
add item (ShoppingCart xs) = ShoppingCart (item:xs)

addToCart :: CartItem -> ShoppingCart -> ShoppingCart
addToCart item (ShoppingCart []) = ShoppingCart [item]
addToCart i@(CartItem product quantity) (ShoppingCart ((CartItem p q): cartList)) 
                | product == p = ShoppingCart (CartItem p (q+quantity) : cartList)
                | otherwise = add (CartItem p q) (addToCart i (ShoppingCart cartList))

stockQuantity :: Stock -> Product -> Int
stockQuantity (Stock xs) p
  | null matches = 0
  | otherwise    = head matches
  where
    matches = [q | (r, q) <- xs, r == p]

checkStock :: Stock -> ShoppingCart -> [Product]
checkStock stock (ShoppingCart items) =
  [ p | CartItem p q <- items, stockQuantity stock p < q ]

createOrder :: Stock -> Customer -> ShoppingCart -> Either Error Order
createOrder stock customer c@(ShoppingCart xs)
    | null missing = Right (Order customer c totalPrice Pending)
    | otherwise    = Left (Error missing)
    where
        missing    = checkStock stock c
        totalPrice = calculateOrderTotal (Order customer c 0 Pending)

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

--IO
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delimiter (x:xs)
    | x == delimiter = "" : rest
    | otherwise = (x : head rest) : tail rest
  where
    rest = splitOn delimiter xs


listToString :: Show a => [a] -> String
listToString = foldr (\x acc -> show x ++ "\n" ++ acc) ""

formatCustomer :: Customer -> String
formatCustomer (Customer id name loyaltyLevel) = show id ++ "," ++ name ++ "," ++ show loyaltyLevel

saveCustomersToHandle :: Handle -> [Customer] -> IO ()
saveCustomersToHandle handle = mapM_ (hPutStrLn handle . formatCustomer)

saveCustomersToFile :: FilePath -> [Customer] -> IO ()
saveCustomersToFile filepath customer = 
    withFile filepath WriteMode $ \handle -> 
        saveCustomersToHandle handle customer

parseCustomerLine :: String -> Customer
parseCustomerLine line = Customer id name loyaltyLevel
  where
    [idStr, name, loyaltyLevelStr] = splitOn ',' line
    id = read idStr :: Int
    loyaltyLevel = read loyaltyLevelStr :: LoyaltyLevel

readCustomer :: FilePath -> IO [Customer]
readCustomer fp = do
  contents <- readFile fp
  forceCustomers (map parseCustomerLine (lines contents))
  where
    forceCustomers :: [Customer] -> IO [Customer]
    forceCustomers []     = return []
    forceCustomers (c:cs) = do
      c `seq` forceCustomers cs
      return (c:cs)

formatStock :: Stock -> String
formatStock (Stock items) = unlines [show id ++ "," ++ name ++ "," ++ printf "%.2f" price ++ "," ++ show category ++ "," ++ show quantity | (Product id name price category, quantity) <- items]

saveStockToHandle :: Handle -> Stock -> IO ()
saveStockToHandle handle stock = hPutStrLn handle (formatStock stock)

saveStockToFile :: FilePath -> Stock -> IO ()
saveStockToFile filepath stock = 
    withFile filepath WriteMode $ \handle -> 
        saveStockToHandle handle stock

parseProductQuantityLine :: String -> (Product, Int)
parseProductQuantityLine line = (Product id name price category, qty)
  where
    [idStr, name, priceStr, categoryStr, qtyStr] = splitOn ',' line
    id = read idStr :: Int
    price = read priceStr :: Float
    category = read categoryStr :: Category
    qty = read qtyStr :: Int


readStock :: FilePath -> IO Stock
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

formatCartItem :: CartItem -> String
formatCartItem (CartItem (Product id name price category) qty) =
    show id ++ "," ++ name ++ "," ++ printf "%.2f" price ++ "," ++ show category ++ "," ++ show qty

formatPriceStatus :: Float -> Status -> String
formatPriceStatus totalPrice status = 
    "Price " ++ printf "%.2f" totalPrice ++ "," ++ show status

saveOrderToHandle :: Handle -> Order -> IO ()
saveOrderToHandle handle (Order cust (ShoppingCart items) totalPrice status) = do
    hPutStrLn handle ("Customer " ++ formatCustomer cust)
    mapM_ (hPutStrLn handle . formatCartItem) items
    hPutStrLn handle (formatPriceStatus totalPrice status)

saveOrdersToFile :: FilePath -> [Order] -> IO ()
saveOrdersToFile filepath orders = 
    withFile filepath WriteMode $ \handle ->
        mapM_ (saveOrderToHandle handle) orders

parsePriceStatusLine :: String -> (Float, Status)
parsePriceStatusLine line = (totalPrice, status)
  where
    [priceStr, statusStr] = splitOn ',' (drop 6 line)
    totalPrice = read priceStr :: Float
    status = read statusStr :: Status


readCartItems :: [String] -> ([CartItem], [String])
readCartItems [] = ([], [])
readCartItems (line:rest)
    | take 5 line == "Price" = ([], line:rest)
    | otherwise = (CartItem product qty : items, remaining)
  where
    (items, remaining) = readCartItems rest
    (product, qty) = parseProductQuantityLine line

  
readOrderFromLines :: [String] -> (Order, [String])
readOrderFromLines (customerLine:rest) = (order, remaining)
  where
    customer = parseCustomerLine (drop 9 customerLine) -- Remove "Customer " prefix
    (cartItems, priceAndRest) = readCartItems rest
    (totalPrice, status) = parsePriceStatusLine (head priceAndRest)
    shoppingCart = ShoppingCart cartItems
    order = Order customer shoppingCart totalPrice status
    remaining = tail priceAndRest


readAllOrdersFromLines :: [String] -> [Order]
readAllOrdersFromLines [] = []
readAllOrdersFromLines lines = order : readAllOrdersFromLines remaining
  where
    (order, remaining) = readOrderFromLines lines


readOrders :: FilePath -> IO [Order]
readOrders filepath = do
  contents <- readFile filepath
  forceList (readAllOrdersFromLines (lines contents))
  where -- strictly evaluate head, then recurse to force the entire list
    forceList :: [Order] -> IO [Order]
    forceList []     = return []
    forceList (o:os) = do
      o `seq` forceList os
      return (o:os)

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

-- Product search I/O -- derive catalog from Stock
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

-- Add to cart I/O -- use addToCart from 5
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
--addToCartHelper pid qty catalog cart = addToCartMaybe (findProductById pid catalog) qty cart
addToCartHelper pid qty catalog = addToCartMaybe (findProductById pid catalog) qty

addToCartMaybe :: Maybe Product -> Int -> ShoppingCart -> IO ShoppingCart
addToCartMaybe Nothing _ cart =
  do putStrLn "Product not found."
     return cart
addToCartMaybe (Just p) qty cart =
  do putStrLn "Product added to cart."
     return (addToCart (CartItem p qty) cart)

-- Customer identification (new vs existing)
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

-- Place order using createOrder (from 7) and update files
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
  orders    <- readOrders "orders.txt"

  (customer, customers') <- identifyCustomer customers

  cartHandler (shopLoop stock (ShoppingCart [])) customer customers' stock orders
  where
    cartHandler action cust customers' st orders = do
      (finalCart, stockBeforeOrder) <- action
      handleCart finalCart cust customers' stockBeforeOrder orders

    handleCart (ShoppingCart []) _ customers' stockBeforeOrder orders = do
      putStrLn "Your cart is empty. Nothing to do."
      saveCustomersToFile "customers.txt" customers'
      saveStockToFile     "stock.txt"     stockBeforeOrder
      saveOrdersToFile    "orders.txt"    orders
      putStrLn "Goodbye."

    handleCart cart cust customers' stockBeforeOrder orders = do
      handleOrder (placeOrderIO stockBeforeOrder cust cart)
                  cart cust customers' stockBeforeOrder orders

    handleOrder action cart cust customers' stockBeforeOrder orders = do
      result <- action
      orderResult result cart cust customers' stockBeforeOrder orders

    orderResult (Left _) _ _ customers' stockBeforeOrder orders = do
      putStrLn "Order failed. No changes saved (except new customers)."
      saveCustomersToFile "customers.txt" customers'
      saveStockToFile     "stock.txt"     stockBeforeOrder
      saveOrdersToFile    "orders.txt"    orders

    orderResult (Right newOrder) cart _ customers' stockBeforeOrder orders = do
      putStrLn "Updating files..."
      saveCustomersToFile "customers.txt" customers'
      saveStockToFile     "stock.txt"     (updateStockWithCart stockBeforeOrder cart)
      saveOrdersToFile    "orders.txt"    (orders ++ [newOrder])
      putStrLn "Order placed and data saved. Thank you!"

--main :: IO ()
--main = customerSession

--12
updateOrders :: [Order] -> [Order] -> Status -> [Order]
updateOrders toUpdate orders newStatus = map update orders
    where
        update :: Order -> Order
        update order
            | order `elem` toUpdate = fromMaybe order (updateOrderStatus order newStatus)
            | otherwise             = order


ordersById :: Int -> [Order] -> IO [Order]
ordersById id currentAllOrders
        | null userOrders = do
                putStrLn "No orders by user"
                searchById currentAllOrders -- let user search again
        | otherwise = do
                        newAllOrders <- return $ updateOrders userOrders currentAllOrders Processing
                        newAllOrders <- return $ updateOrders userOrders newAllOrders Shipped
                        putStrLn $ "Orders of Customer " ++ show id ++ " updated"
                        start newAllOrders -- go back to start with updated orders
        where userOrders = searchOrders [ById id] currentAllOrders
ordersByLL :: LoyaltyLevel -> [Order] -> IO [Order]
ordersByLL ll currentAllOrders
        | null usersOrders = do
                putStrLn "No orders by this Loyalty Level"
                searchByLL currentAllOrders
        | otherwise = do
                        newAllOrders <- return $ updateOrders usersOrders currentAllOrders Processing
                        newAllOrders <- return $ updateOrders usersOrders newAllOrders Shipped
                        putStrLn "Orders updated"
                        start newAllOrders
        where usersOrders = searchOrders [ByLoyaltyLevel ll] currentAllOrders

ordersByHighValue :: [Order] -> [Customer] -> [Order]
ordersByHighValue currentAllOrders customers = updateOrders customersOrders pendingToProcessing Shipped
        where   customersOrders = concat [ searchOrders [ById id] currentAllOrders | (Customer id _ _) <- customers ]
                pendingToProcessing = updateOrders customersOrders currentAllOrders Processing

checkIdInput :: String -> [Order] -> IO [Order]
checkIdInput id currentAllOrders
        | all isDigit id = ordersById (read id :: Int) currentAllOrders
        | id == "b" || id == "B" = startWork currentAllOrders
        | otherwise = do
                putStrLn "ID consist only of digits!"
                searchById currentAllOrders


searchById :: [Order] -> IO [Order]
searchById currentAllOrders = do
        putStrLn "Introduce user ID (b to come back):"
        id <- getLine
        checkIdInput id currentAllOrders

searchByLLInputCheck :: String -> [Order] -> IO [Order]
searchByLLInputCheck ll currentAllOrders
        | ll == "bronze" = ordersByLL Bronze currentAllOrders
        | ll == "silver" = ordersByLL Silver currentAllOrders
        | ll == "gold"   = ordersByLL Gold currentAllOrders
        | ll == "b"      = startWork currentAllOrders
        | otherwise = do
                putStrLn "There is no such Loyalty Level"
                searchByLL currentAllOrders


searchByLL :: [Order] -> IO [Order]
searchByLL currentAllOrders = do
        putStrLn "Introduce user Loyalty Level (Bronze, Silver or Gold. b to come back): "
        ll <- getLine
        searchByLLInputCheck (map toLower ll) currentAllOrders

usersByHighValue :: Float -> [Order] -> IO [Order]
usersByHighValue limit currentAllOrders
        | limit > 0 = do
                putStrLn "Orders updated"
                start $ ordersByHighValue currentAllOrders (highValueCustomers currentAllOrders limit)
        | otherwise = do
                putStrLn "Total price should be non negative number"
                searchByHighValue currentAllOrders
                

searchByHighValueInputCheck :: String -> [Order] -> IO [Order]
searchByHighValueInputCheck value currentAllOrders
        | all isDigit value = usersByHighValue (read value :: Float) currentAllOrders
        | value == "b" || value == "B" = startWork currentAllOrders
        | otherwise = do
                putStrLn "That is not a number"
                searchByHighValue currentAllOrders


searchByHighValue :: [Order] -> IO [Order]
searchByHighValue currentAllOrders = do
        putStrLn "Introduce the minimum total price of not Delivered or Cancelled orders (b to come back):"
        value <- getLine
        searchByHighValueInputCheck value currentAllOrders

startWorkInputCheck :: String -> [Order] -> IO [Order]
startWorkInputCheck answer currentAllOrders
        | answer == "1" = searchById currentAllOrders
        | answer == "2" = searchByLL currentAllOrders
        | answer == "3" = searchByHighValue currentAllOrders
        | answer == "b" = start currentAllOrders
        | otherwise = do
                putStrLn "There is no such option"
                startWork currentAllOrders


startWork :: [Order] -> IO [Order]
startWork currentAllOrders = do 
        putStrLn "What do you want to do?"
        putStrLn "Options:\n1. Search orders by user ID\n2. Search orders by Loyalty Level"
        putStrLn "3. Search orders by high value customers"
        putStrLn "b. Back to main menu"
        answer <- getLine
        startWorkInputCheck answer currentAllOrders

checkStartAnswer :: String -> [Order] -> IO [Order]
checkStartAnswer answer currentAllOrders
        | answer `elem` ["yes", "yeah", "y"] = startWork currentAllOrders
        | answer `elem` ["no", "nope", "n", "nah"] = do
                putStrLn "Understood :("
                return currentAllOrders
        | otherwise = do
                    putStrLn "I do not understand you"
                    start currentAllOrders


start :: [Order] -> IO [Order]
start currentAllOrders = do
        putStrLn "Do you want to continue?"
        answer <- getLine
        checkStartAnswer (map toLower answer) currentAllOrders

ownerSession :: IO ()
ownerSession = do
        putStrLn "Welcome back manager!"
        putStrLn "\nReading orders..."
        loadedOrders <- readOrders "orders.txt"
        putStrLn "Orders loaded:"

        allorders <- start loadedOrders
        putStrLn "Bye, have a good day!"
        -- Save orders
        putStrLn "Saving orders..."
        saveOrdersToFile "orders.txt" allorders
        putStrLn "Orders saved!"

{-
checkPassword :: String -> Bool
checkPassword inputPass
        | inputPass == "IvanAndIsaAreTheBestShopOwners" = do 
                    putStrLn "Access granted."
                    ownerSession
        | otherwise = do
                    putStrLn "Incorrect password. Access denied."
                    session

session :: IO ()
session = do
        putStrLn "Are you a customer (c) or the shop owner (o)? (b to exit)"
        answer <- getLine
        handleAnswer answer
    where
        handleAnswer :: String -> IO ()
        handleAnswer "c" = customerSession
        handleAnswer "o" = do
                putStrLn "Please enter the owner password:"
                inputPass <- getLine
                checkPassword inputPass
        handleAnswer "b" = putStrLn "Goodbye."
        handleAnswer _   = do
                            putStrLn "Unknown option"
                            session
-}
checkPassword :: String -> IO ()
checkPassword inputPass
  | inputPass == "IvanAndIsaAreTheBestShopOwners" = do
      putStrLn "Access granted."
      ownerSession
  | otherwise = do
      putStrLn "Incorrect password. Access denied."
      session

----------------------------------------------------------------
-- Top-level session selector + main
----------------------------------------------------------------

session :: IO ()
session = do
  putStrLn "Are you a customer (c) or the shop owner (o)? (b to exit)"
  answer <- getLine
  handleAnswer answer
  where
    handleAnswer :: String -> IO ()
    handleAnswer "c" = customerSession
    handleAnswer "o" = do
      putStrLn "Please enter the owner password:"
      inputPass <- getLine
      checkPassword inputPass
    handleAnswer "b" = putStrLn "Goodbye."
    handleAnswer _   = do
      putStrLn "Unknown option"
      session

main :: IO ()
main = session
