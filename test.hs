-- sayHello :: String -> IO ()
-- sayHello x = putStrLn ("Hello, " ++ x ++ "!")

removeFst :: Eq a => a -> [a] -> [a]
removeFst c [] = []
removeFst c (x:xs) 
    | x == c     = xs
    | otherwise  = x:removeFst c xs

count :: Char -> String -> Int
count c [] = 0
count c (x:xs)
    | x == c     = 1 + count c xs
    | otherwise  = count c xs

blowup :: String -> String
blowup = blowup_from 1

blowup_from :: Int -> String -> String
blowup_from count [] = []
blowup_from count (x:xs) = rep count x ++ blowup_from (count + 1) xs

rep :: Int -> Char -> String
rep count x 
    | count < 0  = error "cannot repeat fewer than zero times"
    | count == 0 = []
    | otherwise  = x : rep (count - 1) x

srtStrings :: [String] -> [String]
srtStrings [] = []
srtStrings xs = m : (srtStrings (removeFst m xs)) where m = mnmString xs

mnmString :: [String] -> String
mnmString [] = error "empty list"
mnmString [x] = x
mnmString (x:xs) = min x (mnmString xs)

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring xs ys | prefix xs ys = True
substring xs (y:ys) | substring xs ys = True
substring _ _ = False

lengths :: [[a]] -> [Int]
lengths xs = map length xs

sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)

ldp :: Integer -> Integer
ldp = ldpf primes1

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n 
    | rem n p == 0     = p
    | p^2 > n          = n
    | otherwise        = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n
    | n <= 0       = error "not a positive integer"
    | n == 1       = False
    | otherwise    = ldp n == n

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
    where
    mark :: [Integer] -> Integer -> Integer -> [Integer]
    mark (y:ys) k m
        | k == m       = 0 : (mark ys 1     m)
        | otherwise    = y : (mark ys (k+1) m)

primes2 :: [Integer]
primes2 = sieve [2..]

oddsFrom3 :: [Integer]
oddsFrom3 = 3 : map (+2) oddsFrom3

primes3 :: [Integer]
primes3 = 2 : oddsFrom3

running_product :: Num a => a -> [a] -> [a]
running_product _ [] = []
running_product p (x:xs) = prod : running_product prod xs
    where prod = p * x

prime_prod_plus_one :: [Integer]
prime_prod_plus_one = map (+1) (running_product 1 primes1)

non_prime_prod_plus_one :: [Integer]
non_prime_prod_plus_one = filter (not . prime) prime_prod_plus_one

pdivisors :: Integral a => a -> [a]
pdivisors n = [ d | d <- [1..(n-1)], rem n d == 0]

prime_pairs :: [(Integer, Integer)]
prime_pairs = pairs primes1
    where pairs (p1:p2:ps) | p2 == p1 + 2   = (p1, p2) : pairs (p2:ps)
                           | otherwise      = pairs (p2:ps)

                          
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

splitList :: [a] -> [([a], [a])]
splitList [] = []
splitList (x:xs) = splitPrefix [x] xs 

splitPrefix :: [a] -> [a] -> [([a], [a])]
splitPrefix p [] = []
splitPrefix p (x:xs) = (p, x:xs) : splitPrefix (p++[x]) xs

diff :: Eq a => [a] -> [a] -> [a]
diff [] y = []
diff (x:xs) y
    | x `elem` y  = diff xs y
    | otherwise = x : diff xs y

addElem :: a -> [[a]] -> [[a]]
addElem x = map (x:)

powerList :: [a] -> [[a]]
powerList []     = [[]]
powerList (x:xs) = (powerList xs) ++ addElem x (powerList xs) 

data S = Void deriving (Eq, Show)
empty :: [S]
empty = []

genIntersect :: Eq a => [[a]] -> [a]
genIntersect []       = undefined
genIntersect [x]      = x
genIntersect (x:y:xs) = genIntersect ((intersect x y):xs)

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] y = []
intersect (x:xs) y
    | elem x y    = x : intersect xs y
    | otherwise   = intersect xs y 

genUnion :: Eq a => [[a]] -> [a]
genUnion []         = []
genUnion [x]        = x
genUnion (x:y:xs)   = genUnion ((union x y):xs)

union :: Eq a => [a] -> [a] -> [a]
union [] y = y
union (x:xs) y
    | x `elem` y    = union xs y
    | otherwise     = x : union xs y

stirling :: (Eq a, Num a) => a -> a -> a
stirling n k
    | n == k        = 1
    | k == 1        = 1
    | otherwise     = k * stirling (n - 1) k + stirling (n - 1) (k - 1)

bell :: (Num a, Enum a, Eq a) => a -> a
bell n = sum [stirling n k | k <- [1..n]]

listPartition :: Eq a => [a] -> [[a]] -> Bool
listPartition xs xss =
    not (any null xss) && matchConcat xs xss && areDisjoint xss

matchConcat :: Eq a => [a] -> [[a]] -> Bool
matchConcat [] [] = True
matchConcat [] yss = False
matchConcat xs [] = False
matchConcat xs (ys:yss)
    | containsAll xs ys = matchConcat (deleteAll ys xs) yss
    | otherwise         = False

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll xs [] = True
containsAll xs (y:ys)
    | y `elem` xs   = containsAll (delete y xs) ys
    | otherwise     = False

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys)
    | x == y     = ys
    | otherwise  = y : delete x ys

deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll [] ys = ys
deleteAll (x:xs) ys = deleteAll xs (delete x ys)

areDisjoint :: Eq a => [[a]] -> Bool
areDisjoint [] = True
areDisjoint (xs:xss) = any (disjoint xs) xss

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint [] ys = True
disjoint (x:xs) ys = not (elem x ys) && disjoint xs ys 

-- newtype Set a = Set [a] deriving (Eq, Ord)

-- type Rel a = Set(a, a) 

listpart2equiv ::Ord a => [a] -> [[a]] -> [(a, a)]
listpart2equiv _ [] = []
listpart2equiv dom (xs:xss) = (part2equiv xs) ++ listpart2equiv dom xss

-- Creates an equivalence relation from a partition. Creates a
-- set of all possible pairs from the given set.
part2equiv :: [a] -> [(a, a)]
part2equiv [] = []
part2equiv (x:xs) = (allPairs x xs) ++ part2equiv xs
    where
        allPairs x [] = [(x, x)]
        allPairs x (y:ys) = (x, y) : allPairs x ys

equiv2listpart :: Ord a => [a] -> [(a, a)] -> [[a]]
equiv2listpart _ [] = []
equiv2listpart dom ((x,y):xs) = mergePart x y (equiv2listpart dom xs)

mergePart :: Ord a => a -> a -> [[a]] -> [[a]]
mergePart x y xss = 
    let
        xi = whereIs x xss
        yi = whereIs y xss
    in if xi < 0 && yi < 0 then (if x == y then [x] else [x, y]) : xss
        else if xi == yi then xss
        else if xi < 0 then insertAt yi x xss
        else if yi < 0 then insertAt xi y xss
        else mergeParts xi yi xss

whereIs :: Eq a => a -> [[a]] -> Int
whereIs x xss = whereHelper 0 x xss
    where
        whereHelper :: Eq a => Int -> a -> [[a]] -> Int
        whereHelper n x [] = -1
        whereHelper n x (ys:yss)
            | x `elem` ys  = n
            | otherwise    = whereHelper (n+1) x yss

insertAt :: Int -> a -> [[a]] -> [[a]]
insertAt i x (ys:yss)
    | i == 0        = (x:ys):yss
    | otherwise     = ys : insertAt (i-1) x yss

mergeParts ::  Int -> Int -> [[a]] -> [[a]]
mergeParts i j []   = error "cannot merge parts that do not exist"
mergeParts i j xss'@(xs:xss)
    | i == j        = xss'
    | i > j         = mergeParts j i xss'
    | i == 0        = insertAll xs (j-1) xss
    | otherwise     = mergeParts (i-1) (j-1) xss

insertAll :: [a] -> Int -> [[a]] -> [[a]]
insertAll xs i []   = error "cannot merge parts that do not exist"
insertAll xs i (ys:yss)
    | i == 0        = (xs ++ ys) : yss    -- should not need to nub this
    | otherwise     = ys : insertAll xs (i-1) yss

---- Change

type Part = [Int]
type CmprPart = (Int, Part)

expand ::CmprPart -> Part
expand (0,p) = p
expand (n,p) = 1:expand ((n-1),p)

nextpartition :: CmprPart -> CmprPart
nextpartition (k, (x:xs)) = pack (x-1) ((k+x),xs)

pack :: Int -> CmprPart -> CmprPart
pack 1 (m,xs) = (m,xs)
pack k (m,xs) = if k > m then pack (k-1) (m,xs)
                         else pack k (m-k,k:xs)

generatePs :: CmprPart -> [Part]
generatePs p@(n,[])     = [expand p]
generatePs p@(n,(x:xs)) = (expand p: generatePs(nextpartition p))

part :: Int -> [Part]
part n 
    | n < 1     = error "part arg must be >0"
    | n == 1    = [[1]]
    | otherwise = generatePs (0, [n])

change :: [Int] -> Int -> [Part]
change coins n
    | 1 `elem` coins = error "One cent coin is assumed. Do not supply it" 
    | n < 1          = error "Cannot give change of less than one cent"
    | n == 1         = [[1]]
    | n `elem` coins = generateChange coins (0, [n])
    | otherwise      = generateChange coins (nextCoinPartition coins (0, [n]))

generateChange :: [Int] -> CmprPart -> [Part]
generateChange coins p@(n,[]) = [expand p]
generateChange coins p@(n,(x:xs)) 
    = expand p : generateChange coins (nextCoinPartition coins p)

nextCoinPartition :: [Int] -> CmprPart -> CmprPart
nextCoinPartition [] p = error "Ran out of possible coins (should not happen)"
nextCoinPartition coins (k, (x:xs)) = packCoins (newCoins coins) ((k+x),xs)
    where
        newCoins :: [Int] -> [Int]
        newCoins [] = []
        newCoins coins@(c:cs)
            | c < x     = coins
            | otherwise = newCoins cs

packCoins :: [Int] -> CmprPart -> CmprPart
packCoins [] (m,xs) = (m,xs)
packCoins coins@(c:cs) (m,xs) = if c > m then packCoins cs (m,xs)
                                else packCoins coins (m-c,c:xs)

changeEuro :: Int -> [Part]
changeEuro = change [200, 100, 50, 20, 10, 5, 2]

injs :: [Int] -> [Int] -> [[(Int, Int)]]
injs _ [] = [[]]
injs xs (y:ys) = all_injs y ys xs []

all_injs :: Int -> [Int] -> [Int] -> [Int] -> [[(Int, Int)]]
all_injs _ _ [] _ = []
all_injs y ys (x:xs) xps =
    let
        rem_xs = glue xps xs
        lists = map (\z -> (x, y) : z) (injs rem_xs ys)
    in
        lists ++ (all_injs y ys xs (x:xps))

glue :: [a] -> [a] -> [a]
glue [] xs = xs
glue (x:rev_xs) xs = glue rev_xs (x:xs)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (allPerms x []) (perms xs))

allPerms :: a -> [a] -> [a] -> [[a]]
allPerms x pys [] = [glue pys [x]]
allPerms x pys (y:ys) = glue pys (x:y:ys) : allPerms x (y:pys) ys

-- not convinced this is the prettiest way to do this (see below)
stringCompare :: String -> String -> Maybe Ordering
stringCompare [] [] = Just EQ
stringCompare xs [] = if allAlpha xs then Just GT else Nothing
stringCompare [] xs = if allAlpha xs then Just LT else Nothing
stringCompare (x:xs) (y:ys)
    | not (isAlpha x) || not (isAlpha y) = Nothing
    | otherwise     = case compare x y of
        EQ -> stringCompare xs ys;
        LT -> if allAlpha xs && allAlpha ys then Just LT else Nothing;
        GT -> if allAlpha xs && allAlpha ys then Just GT else Nothing;

stringCompare' :: String -> String -> Maybe Ordering
stringCompare' xs ys = if allAlpha xs && allAlpha ys
    then Just (compare xs ys) else Nothing

-- if we only care about significant characters
stringCompare'' :: String -> String -> Maybe Ordering
stringCompare'' [] []        = Just EQ
stringCompare'' xs []        = Just GT
stringCompare'' [] xs        = Just LT
stringCompare'' (x:xs) (y:ys)
    | isAlpha x && isAlpha y = Just (compare x y)
    | otherwise              = Nothing


allAlpha :: String -> Bool
allAlpha = all isAlpha

isAlpha :: Char -> Bool
isAlpha x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')

data Natural = Z | S Natural
    deriving (Eq, Show)

plus :: Natural -> Natural -> Natural
plus m Z = m
plus m (S n) = S (plus m n)

leq :: Natural -> Natural -> Bool
leq Z _ = True
leq (S _) Z = False
leq (S m) (S n) = leq m n

gt :: Natural -> Natural -> Bool
gt m n = not (leq m n)

lt :: Natural -> Natural -> Bool
lt m n = gt n m

subtr :: Natural -> Natural -> Natural
subtr m Z = m
subtr Z _ = error "underflow"
subtr (S m) (S n) = subtr m n

quotient :: Natural -> Natural -> Natural
quotient m Z = error "div zero"
quotient m n =  if m `lt` n then Z 
                else S (quotient (m `subtr` n) n)

remainder :: Natural -> Natural -> Natural
remainder m Z = error "div zero"
remainder m n = if m `lt` n then m
                else remainder (m `subtr` n) n

catalan :: Int -> Int
catalan 0 = 1
catalan nplus1 = catalan_sum (nplus1 - 1) 0

catalan_sum :: Int -> Int -> Int
catalan_sum 0 m = (catalan 0) * (catalan m)
catalan_sum n m = (catalan n) * (catalan m) + (catalan_sum (n-1) (m+1))

catalans :: [Int]
catalans = map catalan [0..]

data TernaryTree = L | N TernaryTree TernaryTree TernaryTree deriving Show

makeTernaryTree :: Integer -> TernaryTree
makeTernaryTree 0 = L
makeTernaryTree n = N (makeTernaryTree (n-1)) (makeTernaryTree (n-1)) (makeTernaryTree (n-1))

countTT :: TernaryTree -> Integer
countTT L = 1
countTT (N t1 t2 t3) = 1 + countTT t1 + countTT t2 + countTT t3

depthTT :: TernaryTree -> Integer
depthTT L = 0
depthTT (N t1 t2 t3) = 1 + (depthTT t1) `max` (depthTT t2) `max` (depthTT t3)

balancedTT :: TernaryTree -> Bool
balancedTT L = True
balancedTT (N t1 t2 t3) = balancedTT t1 && balancedTT t2 && balancedTT t3
    && depthTT t1 == depthTT t2 && depthTT t2 == depthTT t3

data Tree = Lf | Nd Int Tree Tree deriving Show

insert :: Int -> Tree -> Tree
insert i Lf = Nd i Lf Lf
insert i n@(Nd j l r) = case compare i j of
    EQ -> n;    -- just leave the tree alone if it already has our number
    LT -> Nd j (insert i l) r;
    GT -> Nd j l (insert i r);

-- this works backwards
makeTree' :: [Int] -> Tree
makeTree' [] = Lf
makeTree' (x:xs) = insert x (makeTree' xs)

list2tree :: [Int] -> Tree
list2tree = insertAllTree Lf

insertAllTree :: Tree -> [Int] -> Tree
insertAllTree t [] = t
insertAllTree t (x:xs) = insertAllTree (insert x t) xs

tree2list :: Tree -> [Int]
tree2list Lf = []
tree2list (Nd i l r) = tree2list l ++ [i] ++ tree2list r

contains :: Tree -> Int -> Bool
contains Lf i = False
contains (Nd j l r) i = case compare i j of
    EQ -> True;
    LT -> contains l i;
    GT -> contains r i;

tree_merge :: Tree -> Tree -> Tree
tree_merge l r = insertAllTree l (tree2list r)

-- hard to do this without converting to a list
-- tree_merge t Lf = t
-- tree_merge Lf t = t
-- tree_merge n1@(Nd i1 l1 r1) n2@(Nd i2 l2 r2) = case compare i1 i2 of
--     EQ -> Nd i1 (tree_merge l1 l2) (tree_merge r1 r2);
--     LT -> Nd i2 (tree_merge )

tree_steps :: Tree -> Int -> Int
tree_steps = tree_steps_int 0 where
    tree_steps_int _ Lf _ = -1
    tree_steps_int n (Nd j l r) i = case compare i j of
        EQ -> n;
        LT -> tree_steps_int (n+1) l i;
        GT -> tree_steps_int (n+1) r i;

data Tr a = Nil | T a (Tr a) (Tr a) deriving (Eq, Show)

mapT :: (a -> b) -> Tr a -> Tr b
mapT _ Nil = Nil
mapT f (T x l r) = T (f x) (mapT f l) (mapT f r)

foldn :: (a -> a) -> a -> Natural -> a
foldn h c Z = c
foldn h c (S n) = h (foldn h c n)

foldT :: (a -> b -> b -> b) -> b -> (Tr a) -> b
foldT _ c Nil = c
foldT f c (T x l r) = f x (foldT f c l) (foldT f c r)

preorder_tree2list :: Tr a -> [a]
preorder_tree2list = foldT (\x l r -> [x] ++ l ++ r) []

inorder_tree2list :: Tr a -> [a]
inorder_tree2list = foldT (\x l r -> l ++ [x] ++ r) []

postorder_tree2list :: Tr a -> [a]
postorder_tree2list = foldT (\x l r -> l ++ r ++ [x]) []

ordered :: Ord a => Tr a -> Bool
ordered = check (\ _ -> True) where
    check _ Nil = True
    check cond (T x l r) = cond x && check (<x) l && check (>x) r 

type Dict = Tr (String, String)

lookupD :: String -> Dict -> [String]
lookupD s Nil = []
lookupD s (T (k, v) l r) = case compare s k of
    EQ -> [v];
    LT -> lookupD s l;
    GT -> lookupD s r;

split :: [a] -> ([a], a, [a])
split xs = (ys, y, ys2) where
    ys       = take n xs
    (y:ys2)  = drop n xs
    n        = length xs `div` 2

buildTree :: [a] -> Tr a
buildTree [] = Nil
buildTree xs = 
    let (ys, y, ys2) = split xs
    in T y (buildTree ys) (buildTree ys2)

data LeafTree a = Leaf a | Node (LeafTree a) (LeafTree a) deriving Show

ltree :: LeafTree String
ltree = Node (Leaf "I") (Node (Leaf "Love") (Leaf "You"))


mapLT :: (a -> b) -> LeafTree a -> LeafTree b
mapLT f (Leaf x) = Leaf (f x)
mapLT f (Node l r) = Node (mapLT f l) (mapLT f r)

reflect :: LeafTree a -> LeafTree a
reflect (Leaf x) = Leaf x
reflect (Node l r) = Node (reflect r) (reflect l)

data Rose a = Bud a | Br [Rose a] deriving (Eq, Show)

rose :: Rose Int
rose = Br [Bud 1, Br [Bud 2, Bud 3, Br [Bud 4, Bud 5, Bud 6]]]

mapR :: (a -> b) -> Rose a -> Rose b
mapR f (Bud x) = Bud (f x)
mapR f (Br xs) = Br (map (mapR f) xs)


genUnion' :: Eq a => [[a]] -> [a]
genUnion' = foldr union []


genIntersect' :: Eq a => [[a]] -> [a]
genIntersect' = foldr1 intersect

srt :: Ord a => [a] -> [a]
srt = foldr insrt []

insrt :: Ord a => a -> [a] -> [a]
insrt x [] = [x]
insrt x (y:ys)
    | x < y     = x:y:ys
    | otherwise = y: (insrt x ys)

-- Tower of Hanoi
data Peg = A | B | C
type Tower = ([Int], [Int], [Int])

move :: Peg -> Peg -> Tower -> Tower
move A B (x:xs, ys, zs) = (xs, x:ys, zs)
move B A (xs, y:ys, zs) = (y:xs, ys, zs)
move A C (x:xs, ys, zs) = (xs, ys, x:zs)
move C A (xs, ys, z:zs) = (z:xs, ys, zs)
move B C (xs, y:ys, zs) = (xs, ys, y:zs)
move C B (xs, ys, z:zs) = (xs, z:ys, zs)

transfer :: Peg -> Peg -> Peg -> Int -> Tower -> [Tower]
transfer _ _ _ 0 tower = [tower]
transfer p q r n tower =  transfer p r q (n-1) tower
                       ++ transfer r q p (n-1) (move p q tower')
    where tower' = last (transfer p r q (n-1) tower)

hanoi :: Int -> [Tower]
hanoi n = transfer A C B n ([1..n], [], [])

check :: Int -> Tower -> Bool
check 0 t = t == ([], [], [])
check n (xs, ys, zs)
    | xs /= [] && last xs == n = check (n-1) (init xs, zs, ys)
    | zs /= [] && last zs == n = check (n-1) (ys, xs, init zs)
    | otherwise                = False

maxT :: Tower -> Int
maxT (xs, ys, zs) = foldr max 0 (xs ++ ys ++ zs)

checkT :: Tower -> Bool
checkT t = check (maxT t) t
