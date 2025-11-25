import Prelude hiding (product)

data Category = Electronics | Books | Clothing | Groceries deriving (Show, Eq)

data Product = Product {pid :: Int, pname :: String, price :: Float, category :: Category}
instance Show Product where
    show (Product id name price category) = "Product id: " ++ show id ++ ", product: " ++ show name ++ ", price: " ++ show price ++ ", category: " ++ show category
--
data LoyaltyLevel = Bronze | Silver | Gold deriving (Show, Eq)

data Customer = Customer {cid :: Int, cname :: String, loyaltyLevel :: LoyaltyLevel}

instance Show Customer where
    show (Customer id name loyaltyLevel) = "Customer id: " ++ show id ++ ", customer: " ++ show name ++ ", loyalty level: " ++ show loyaltyLevel
--
data CartItem = CartItem {product :: Product, quantity :: Int}

instance Show CartItem where
    show (CartItem product quantity) = "Item: " ++ show product ++ ", units: " ++ show quantity
--
newtype ShoppingCart = ShoppingCart [CartItem]

instance Show ShoppingCart where
    show (ShoppingCart items) = "Shopping cart:\n" ++ unlines (map (\x -> "\t" ++ show x) items)
--
newtype Stock = Stock [(Product, Int)] -- product and their available quantity

instance Show Stock where
    show (Stock items) = "Stock:\n" ++ unlines (map (\(p, i) -> "\t" ++ show p ++ ", quantity available: " ++ show i) items)

data Status = Pending | Processing | Shipped | Delivered | Cancelled deriving Show
--
data Order = Order {customer :: Customer, shoppingCart :: ShoppingCart, totalPrice :: Float, status :: Status}

instance Show Order where
    show (Order customer shoppingCart totalPrice status) = "Order for customer: " ++ show customer ++ "\n" ++ show shoppingCart 
        ++ "Total price: " ++ show totalPrice ++ ", status: " ++ show status ++ "\n"


data SearchCriterion = ById Int | ByLoyaltyLevel LoyaltyLevel | ByProductId Int | ByCategory Category | ByTotalPrice Float deriving Show
--
calculateProductPrice :: Product -> Float
calculateProductPrice (Product _ _ p _) = p
--
class Discountable a where
  applyDiscount :: a -> Float -> Float

instance Discountable LoyaltyLevel where
  applyDiscount Bronze p = p
  applyDiscount Silver p = p * 0.95
  applyDiscount Gold   p = p * 0.90


instance Discountable Category where
  applyDiscount Books p = p * 0.85 -- Books 15% off
  applyDiscount _     p = p


calculateOrderTotal :: Order -> Float
calculateOrderTotal (Order cust (ShoppingCart items) _ _) =
  sum [ fromIntegral (quantity it) * priceAfter (product it) | it <- items ]
  where
    priceAfter prod = applyDiscount (loyaltyLevel cust)(applyDiscount (category prod)(calculateProductPrice prod))
-- 5
instance Eq Product where
        (Product pid1 _ _ _) == (Product pid2 _ _ _) = pid1 == pid2
add :: CartItem -> ShoppingCart -> ShoppingCart
add item (ShoppingCart xs) = ShoppingCart (item:xs)

addToCart :: CartItem -> ShoppingCart -> ShoppingCart
addToCart item (ShoppingCart []) = ShoppingCart [item]
addToCart i@(CartItem product quantity) (ShoppingCart ((CartItem p q): cartList)) 
                | product == p = ShoppingCart (CartItem p (q+quantity) : cartList)
                | otherwise = add (CartItem p q) (addToCart i (ShoppingCart cartList))
--EXAMPLES
product1 = Product 1 "Ipad" 12 Electronics
product2 = Product 2 "Blouse"  15 Clothing
product3 = Product 3 "Lemons"  5 Groceries
productBook = Product 4 "Functional Programming for Dummies 101" 40 Books

catalog :: [Product]
catalog = [product1, product2, product3, productBook]

stock = Stock (zip [product1, product2, product3, productBook] [2, 3, 4, 2])
--6
stockQuantity :: Stock -> Product -> Int
stockQuantity (Stock xs) p
  | null matches = 0
  | otherwise    = head matches
  where
    matches = [q | (r, q) <- xs, r == p]

checkStock :: ShoppingCart -> [Product]
checkStock (ShoppingCart items) = [ p | CartItem p q <- items, stockQuantity stock p < q ]

--7
newtype Error = Error [Product] deriving Show

createOrder :: Customer -> ShoppingCart -> Either Error Order
createOrder customer c@(ShoppingCart xs) 
        | null missing = Right (Order customer c totalPrice Pending)
        | otherwise = Left (Error missing)
        where 
                missing = checkStock c
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
instance Eq Customer where
  (Customer id1 _ _) == (Customer id2 _ _) = id1 == id2

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
-- 11
--helper 1 - search by name
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
  [ p | p@(Product _ _ price _) <- ps, price <= maxP ]
  
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
-- 11 part a - all search I/O functions
searchProductIO :: [Product] -> IO ()
searchProductIO catalog = do
  putStrLn "Enter product name:"
  name <- getLine
  printProducts (searchProductsByName name catalog)
  
searchCategoryIO :: [Product] -> IO ()
searchCategoryIO catalog = do
  putStrLn "Enter category (Electronics, Books, Clothing, Groceries):"
  s <- getLine
  handleReadCategory (readCategory s) catalog

handleReadCategory :: Maybe Category -> [Product] -> IO ()
handleReadCategory Nothing _ = putStrLn "Unknown category."
handleReadCategory (Just cat) catalog =
  printProducts (searchProductsByCategory cat catalog)

searchMaxPriceIO :: [Product] -> IO ()
searchMaxPriceIO catalog = do
  putStrLn "Enter maximum price:"
  s <- getLine
  printProducts (searchProductsByMaxPrice (read s) catalog)
-- 11 part b - adding items 2 cart
addToCartIO :: [Product] -> ShoppingCart -> IO ShoppingCart
addToCartIO catalog cart = do
  putStrLn "Enter product ID:"
  sId <- getLine
  putStrLn "Enter quantity:"
  sQty <- getLine
  addToCartHelper (read sId) (read sQty) catalog cart
  
-- for 11 b
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
-- 11 part c - place order
placeOrderIO :: Customer -> ShoppingCart -> IO ()
placeOrderIO cust cart =
  handleOrderResult (createOrder cust cart)

handleOrderResult :: Either Error Order -> IO ()
handleOrderResult (Left err) =
  putStrLn ("Order could not be created: " ++ show err)
handleOrderResult (Right order) = do
  putStrLn "Order created successfully:"
  print order

-- TESTING CART
initialCart = ShoppingCart []

customerSilver = Customer 2 "Isa"  Silver
cart1 = CartItem product1 1
cart2 = CartItem product2 1
cartBook2   = CartItem productBook 1
cartMixed = ShoppingCart [cart1, cart2, cartBook2]



main = do
  --putStrLn "Search by"
  --searchProductIO catalog  
  --searchMaxPriceIO catalog
  --searchCategoryIO catalog
  --putStrLn "Add product to cart:"
  --cart <- addToCartIO catalog initialCart
  --print cart
  putStrLn "Placing order:"
  placeOrderIO customerSilver cartMixed


