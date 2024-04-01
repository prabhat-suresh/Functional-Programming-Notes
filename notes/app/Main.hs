module Main where

main :: IO ()

-- main = putStrLn "Hello, Haskell!"

fst (x1, _) = x1 -- wildcard

len [] = 0
len (_ : xs) = 1 + len xs

mysum [] = 0
mysum (x : xs) = x + mysum xs

isEmpty [] = True
isEmpty _ = False

mytake _ [] = []
mytake num (x : xs) =
  if num > 0
    then x : mytake (num - 1) xs
    else []

mymap f [] = []
mymap f (x : xs) = f x : mymap f xs

-- f :: s -> a -> s
myfoldl f summ [] = summ
myfoldl f summ (x : xs) = myfoldl f (f summ x) xs

-- f :: a -> s -> s
myfoldr f summ [] = summ
myfoldr f summ (x : xs) = f x (myfoldr f summ xs)

-- zipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f (x : xs) (y : ys) = f x y : myzipWith f xs ys
myzipWith _ _ _ = []

data BinTree a
  = Empty
  | Node (BinTree a) a (BinTree a)

sampleTree = Node (Node (Node Empty 4 Empty) 2 (Node Empty 5 Empty)) 1 (Node (Node Empty 6 Empty) 3 (Node Empty 7 Empty))

zeroes = 0 : zeroes

-- main = print zeroes

isEmptyTree Empty = True
isEmptyTree _ = False

inorder Empty = []
inorder (Node left root right) = inorder left ++ (root : inorder right)

inorderHelper Empty ls = ls
inorderHelper (Node left root right) ls = inorderHelper left (root : inorderHelper right ls)

-- main = print (inorderHelper sampleTree [])

rotateAC :: BinTree a -> BinTree a
rotateAC (Node t1 root (Node t2 root2 t3)) = Node (Node t1 root2 t2) root t3
rotateAC whateverElse = whateverElse

-- importing notes

--  Functional Programming
--   2024-01-17 10:02
--   20:20:60 marks distribution

--   {https://bitbucket.org/piyush-kurur/functional-programming/src/master/}[course website]

--  Haskell
--  GHC compiler
--  Tool to look into: Cabal

--   has an interpreter called hugs

--  ghci - interactive mode for ghc
--     not an interpreter but a REPL

--     compile using ghc --make <filename>

--  number of words in a file
--    words :: String -> [String] (list of strings)
--    length :: [a] -> Int         (a is a type variable)

--    2024-01-31 10:04

--    wc str = length (words str)

--    alternatively       wc = length . words      (function composition)

-- ___

--  Haskell
--  Strongly typed and statically typed
--  supports parametric polymorphism
--  principled ad hoc polymorphism

--    Basic types : Int, Char, Bool, String  etc    (all start with capital letters)

--    all variables (including type variables) start with lowercase

--    Convention: camel case

--    [2,3,5] is 2:3:5:[]    (: is cons and :: is is of type opposite of SML)

-- ___
-- 2024-02-02 10:01

--  Introduction rules
--  List
--  Nil rule:

--    ___
--     []::[a]

--  Cons rule:

--     x::a
--     xs::[a]
--     ___
--     (x:xs)::[a]

--  associativity
--   function composition associates towards right
--   a . b . c is same as a . (b . c)
--   function application associates towards left
--   a -> b -> c is same as (a -> b) -> c
-- ___

-- 2024-02-03 10:59

--  Pattern matching
--  Patterns are essentially a subclass of expressions
--  They are linear: no variable is repeated
--  Wild card is a pattern

-- pattern matching is purely matching. Not in terms of equality. (To check if two expressions are the same is an undecidable problem)

--  fold
--  foldr
--     o :: a -> s -> s
--     (a1 o (a2 o (....(an o s0))))
-- ___

-- 2024-02-07 10:01

--  what fold does
--     :           --fold-->             f
--   /                                x1 f
-- x1      :                             x2 f
--       x2  :                             .
--     .                                   .
--     .                                   .
--     .

--   foldr :: (a->s->s) -> s -> [a] -> s

--   List constructors: replace [a] with s
--   s0 :: s and cf :: a->s->s

--   analyzing the constructors gives the type of fold

--  infix operators
--   (+) : binary operator Int -> Int -> Int
--   (+1) take x and gives x+1
--   (2+) takes x and gives 2+x
--   5 `mod` 2 is equivalent to mod 5 2 (use backtick for infix)

--  Lazy Evaluation
--   opposite: eager evaluation

--   e1 e2

--   only evaluate when required and only as much computation as required

--   advantages:
--   ones=1:ones   (list of infinite ones)

--   foo x y = if x<y then e1 else y1
--   where y1 = very huge computation

-- ___
-- 2024-02-09 10:02

myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

tl :: [a] -> [a]
tl (x : xs) = xs

fib = 1 : 1 : zipWith (+) fib (tl fib)

firstN _ [] = []
firstN n (x : xs) =
  if n > 0
    then x : firstN (n - 1) xs
    else []

-- main = print (firstN 20 fib)

--   [1,1,2,3,5,...]   (fib)
-- + [1,2,3,5,...]     (tl fib)
--   ___
--   [2,3,5,8,...]     (tl (tl fib))

--  Sieve
primes = sieve [2 ..]

sieve (x : xs) = x : sieve (strikeoff x xs)

-- strikeoff x xs = remove (multiples x) xs
-- or better:
strikeoff x = remove (multiples x)

multiples x = [k * x | k <- [2 ..]]

remove rm@(r : rs) f@(y : ys)
  | y > r = remove rs f
  | y Prelude.== r = remove rs ys
  | otherwise = y : remove rm ys

-- main = print (firstN 20 primes)

--  Question pretty format [[string]] by padding accordingly using just one iteration through the list
-- ___

--    2024-02-12 10:03

type Row = [String]

type Table = [Row]

--    To pad each word with spaces to make its length the same as all the other words in that column.

--    alignTable padding tab = map (padrow padding) tab
--    alignRow paddin row = zip align padding row
--    align n s = s ++ repeat ' ' (n-length s)           (++ is concatenation)

--    zip rows with padding of the previous rows and max gives the padding required for the table

--    keeping the padding as an infinite list is convenient for zip

--    zeroes: 0:zeroes
--    padRow r=map length r ++ zeroes
--    padTable = foldr combfn zeroes
--    combfn: Row -> [Int] -> [Int]
--    combfn = zipwith (s n = max n (length s))

--    Write a function that traverses only once with the specs as:
--    Input: (1) a padding (list)  and (2) the table
--    Output: (alignTable padding tab,padTable tsb)

--    The amazing part of lazy evaluation is used when calling the function, say foo.

--    let (padded table,padding) = foo padding table in padded table

--  This indeed happens in just one iterarion
-- ___

--  2024-02-14 10:11
--  foldl vs foldr
--  In eagerly evaluated languages foldl will be more efficient than foldr due to tail call optimization.
--  but in lazy evaluated ones the computation anyway will be deferred by using data structure called thunks (which have a pointer to a computation). What is not stored in the heap for recursion (haskell, SML, etc don't use stack for receursion) will be used for evaluation.
--  use stricter version of foldl for most purposes

--  Algebraic datatypes
--  data Tuple a b = T a b
--  a and b are type variables. Tuple a b is a new type with a single constructor T::a->b->Tuple a b
--  fstT (T x _)=x
-- ___
--  2024-02-17 14:05
--  in haskell generally preferred to use curried style for types and starts in uppercase

-- data BinTree a
--   = Empty
--    | Node (BinTree a) a (BinTree a)

-- inorder Empty = []
-- inorder (Node left root right) = inorder left ++ (root : inorder right)
-- ___

--  2024-02-19 10:35
--  Question: convert a binary tree into another one with all node values replaced with minimum value in one pass

--  data Maybe a = Just a | Nothing

--  data Either a b = Left a | Right b
--  usually to return something like Either err a (Conventionally right is right :) )
--  hd :: [a] -> Maybe a
--  the correct head function
--      hds (x:_) = Just x
--      hds [] = Nothing
-- ___
--  2024-02-21 10:10
--  Type classes: A principled approach to parametric polymorphism
--  the head function above uses parametric polymorphism to ensure that whatever is returned is an element of the list if it returns Just x
--  overloading rather. examples like (+)
--  equality cannot be parametrically polymorphic

class MyEq a where
  (==) :: a -> a -> Bool

--   creates a class in the world of types
--   an instance of this class can be created as:

instance MyEq Bool where
  (==) True True = True
  (==) False False = True
  (==) _ _ = False

--  :type (==)          :: MyEq a => a->a->Bool
--  here the type is of the form:      constraints => tau
--        it's now possible to write foo x y = if x == y then "equal" else "not equal"

--        foo :: MyEq a => a->a->string (haskell figures out the type)

instance (MyEq a, MyEq b) => MyEq (a, b) where
  (==) (a1, b1) (a2, b2) = a1 Main.== a2 && b1 Main.== b2

--        similarly for lists

instance (MyEq a) => MyEq [a] where
  (==) (x : xs) (y : ys) = x Main.== y && xs Main.== ys
  (==) [] [] = True
  (==) _ _ = False

-- ___
--  2024-02-23 10:13
--  default definitions:
-- class MyEq a where
--   (==) :: a->a->Bool
--   (/=) :: a->a->Bool
--   (/=) x y = not (x==y)      -- default definition of not equal to applies, unless specifically defined as an instance

--  Hash consing
--  Hash is a type
--  hash :: String -> Hash
--  data HString = HString Hash String      (will be hidden from the user in later stages so that arbitrary instances of HString can't be created)
--       fromString :: String -> HString
--       fromString str = HString (hash str) str
--       toString :: HString -> string
--       toString (HString _ s) = s

--  instance MyEq HString where                  (here we needn't mention MyEq Hash and MyEq String as constraints. Constraints are required only when we are defining over type variables)
--        (==) (HString h1 s1) (HString h2 s2) = h1==h2 && s1==s2

-- ___
--  2024-02-26 10:02

--  constrained classes
class (MyEq a) => MyOrd a where -- (mentioning at the class level itself that the class ord has the constraint of MyEq)
  (<=) :: a -> a -> Bool

instance MyOrd Bool where
  (<=) True False = False
  (<=) _ _ = True

instance (MyOrd a, MyOrd b) => MyOrd (a, b) where
  (<=) (x1, y1) (x2, y2)
    | x1 Main.== x2 = y1 Main.<= y2
    | x1 Main.<= x2 = True
    | otherwise = False

--    type inferred will be :: (MyOrd a,MyOrd b) => (a,b) -> (a,b) ->Bool

instance (MyOrd a) => MyOrd [a] where
  (<=) (x : xs) (y : ys)
    | x Main.== y = xs Main.<= ys
    | x Main.<= y = True
    | otherwise = False
  (<=) (x : xs) _ = False
  (<=) _ _ = True

sort :: (MyOrd a) => [a] -> [a]

partition [] _ = ([], [])
partition (x : xs) pr =
  let (l, r) = partition xs pr
   in if pr x then (x : l, r) else (l, x : r)

sort [] = []
sort (x : xs) = sort l ++ (x : sort r) where (l, r) = partition xs (Main.<= x)

instance MyEq Int where
  (==) = (Prelude.==)

instance MyOrd Int where
  (<=) = (Prelude.<=)

main = print (sort [2 :: Int, 5, 3, 7, 4, 9, 8, 1, 0])

-- ___

--  2024-02-28 10:03

--  ord should give a total order (x <= y or (inclusive) y<=x) (divisibility is an example of a partial order that's not a total order)
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

-- deriving (Enum)

--  For Eq instance we would have to write 7 lines
--  For Ord it will take n^2 lines
--  A hack will be to write a function from Day to Int and use the MyOrd of Int and n lines of pattern matching
--  There is a class Enum in the standard library
--  An easier way is by using the deriving feature

-- data Foo
--   = Bar String
--   | Biz Int
--   deriving (Eq, Ord, Enum)

-- (make sure to add MyEq even if using just MyOrd as MyEq is required by MyOrd)

--  Ordering will be according to the order of declaration of constructors i.e., Bar _ always <= Biz _
-- Not sure about the above -  deriving Enum for Foo shows error

newtype Foo a = Foo a
  deriving (Eq)

--  the equivalent way of writing the instance would be:
--  instance MyEq a => MyEq (Foo a) where
--        (==) (Foo x) (Foo y) = x==y

--  deriving makes it easy by avoiding writing verbose code

--  Coherence of instance definition:
--  suppose two instances of a class like MyOrd are defined in two separate libraries and both are included in a codebase
--  if both definitions are defined differently, it will result in problems
-- ___

--  2024-03-01 10:03
--    Functors Applicative Monad
class MyFunctor t where -- (t is the functor here)
  fmap :: (a -> b) -> t a -> t b

--      can think of list as function from type to type      list:: Type -> Type
--      takes type a and gives list of a

--      [] :: Type -> Type
--      [] Int = [Int]

instance MyFunctor [] where -- (indentation is important)
-- fmap :: (a -> b) -> [a] -> [b] -- (as [] a = [a])
  fmap = map

instance MyFunctor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just x) = Just (f x) -- (or can write it as:   Just $ f a  where ($) :: (a -> b) -> a -> b)
  fmap _ Nothing = Nothing

instance MyFunctor BinTree where
  -- fmap :: (a -> b) -> BinTree a -> BinTree b
  fmap f Empty = Empty
  fmap f (Node left node right) = Node (Main.fmap f left) (f node) (Main.fmap f right)

--  Some laws to be followed by Functors (cannot be enforced in the language):
--  fmap id =id
--  fmap f . fmap g = fmap (f . g)

-- ___

--  2024-03-04 10:22

--   Functors can be abstracted out as lists just like force is abstracted out as a vector as long as they obey the laws

--  IO Functor (available in stdlib)
--  type of i/o actions that result in a value of type a
--     getLine :: IO String
--     read :: Read a => String -> a

--  (read "123" :: Int)   Type has to be mentioned to give Int
--  write function getIntLine :: IO Int
--       foo=fmap read           foo:: (Read a, Functor t) => t string -> t a
--       getIntLine = foo getLine      (as getLine guarantees IO functor)

--  newtype is generally preferred over data if there is only a single constructor
--        newtype Person=Person String instead of
--        data    Person=Person String

--   write getPersonLine :: IO Person
--         getPersonLine=fmap Person getLine

-- ___
--  2024-03-06 10:03

--  Applicative
--    data Person = Person String Int          (Person :: String -> Int -> Person)
--    get = fmap Person getLine
--    get :: IO (Int -> Person)

--    cannot apply the function (a->b) on a as it is hidden in t a and cannot be accessed as it is

--  class Functor t => Applicative t where
--     extends functors to multiple arguments
--  pure :: a -> t a       (zero side effect is also a side effect)
--  (<>):: t (a->b) -> t a -> t b
--  get = fmap Person getLine <> getIntLine
--       add3 :: Int -> Int -> Int -> Int
--       add3 x y z = x+y+z

--       get = fmap add3 getIntLine <> getIntLine <> getIntLine

--  use <$> to make code clearer and make it look just like function application
--        f <$> (t x) = fmap f (t x)
--        now get will be = Person <$> getLine <> getIntLine

--  think about the following:
--    instance Applicative Maybe where
--         pure =  Just
--         <> (Just f) (Just x) = Just (f x)
--         <> _ _ = Nothing
-- ___
--  2024-03-08 10:06

--  data Exp = C Int | PLUS Exp | PLUS Exp
--  eval :: Exp -> (Maybe Int)
--     eval (C x) = Just x
--     eval (PLUS e1 e2) = case eval e1 of
--                               Just x -> case eval e2 of
--                                         Just y -> Just (x+y)
--                                         Nothing -> Nothing
--                               Nothing -> Nothing

--     eval (DIV e1 e2) =
--      can be written much more succinctly using the Applicative

--         f <$> ma = pure f <> ma

--         eval (PLUS e1 e2) = (+) <$> (eval e1) (eval e2)

--  instance Applicative [] where
--  pure :: a -> [a]
--   pure x = [x]
--  (<>) :: [a->b] -> [a] -> [b]

--  fs <> xs = [f x | f <- fs, x -> xs]

-- ___
--  2024-03-11 10:14

--   as we can't have two instances of an Applicative, Functor etc, we have to define a new type to have an Applicative for Ziplist operation
--  newtype Ziplist a = Ziplist [a]
--  instance Functor Ziplist where
--  fmap :: (a->b) -> Ziplist a -> Ziplist b
--  fmap f (Ziplist someList) = Ziplist (map f someList)
--  instance Applicative Ziplist where
--  (<>) :: Ziplist (a->b) -> Ziplist a -> Ziplist b
--  (<>) (Ziplist fs) (Ziplist xs) = Ziplist (zipWith ($) fs xs)
--  pure :: a-> Ziplist a
--  pure x = Ziplist xs where xs = x:xs
--  pure has to give an infinite list as if we define pure x = Ziplist [x] then whenever we zip with it the result will be trimmed down to a single element
-- ___
--  2024-03-13 10:06
--  Overview
--  Functor - Applying pure functions "inside" the data structure
--     map operator (<$>) :: (a->b) -> t a -> t b
--  Applicative / Monoidal Functors - Extending Functor interface for "multi-argument" function
--     apply operator (<>) :: t (a->b) -> t a -> t b
--  Monad
--     bind operator (>>=) :: t a -> (a -> t b) -> t b
--  (1) Read an int, read a string,...
--  (2) Read an int n, if n is read then read n strings
--      notice the dependency in sequencing on previous actions. This is not captured by the applicative as it assumes no dependency
--      and can compute t (a->b) and t a in parallel

--  class Applicative t => Monad t where
--    return :: a -> t a       (legacy stuff and can be ignored; is exactly same as pure)
--    (>>=) :: t a -> (a -> t b) -> t b

--    (>>) :: ta -> tb -> tb
--    (>>) ta tb = (>>=) ta (_ -> tb)

--    historically they realised that the apply operator can be written in terms of the bind operator

--    (<>) tf ta = tf >>= (f -> ta >>= (x -> pure (f x)))

--  The do-notation       to make it more readable
--  stmt = do action; stmt       equivalent to  action >> stmt
--  | do x <- action; stmt       equivalent to action >>= (x -> stmt)
--  | action                     equivalent to action
--      can rewrite apply operator as
--      (<>) tf ta = do f <- tf
--                       x <- ta
--                       return (f x)
-- ___
--  2024-03-15 10:06
--  print :: Show a => a -> IO()
--  class Show a where
--     show :: a -> string

--  given putStr :: string -> IO()
--     print x = putStr ( show x)
--     print = putStr . show

--  getLine :: IO string
--      get :: Read a => IO a
--      get = fmap read getLine
--      get = read <$> getLine

--  class Read a where
--       read :: String -> a

--  a simple program which has an imperative programming feel using monads
--     do putStr "Enter an integer: "
--         inp <- get
--         print (inp + 1)

--  infinite program to keep printing the squares of input

--    sq_input = (do putStr "Enter an integer: "
--         inp <- get
--         print (inp  inp)
--         sq_input)

--  haskell programs only execute the main function
--  for all IO, the main function is sufficient and the Haskell compiler evaluates the main function (that is the plan!)
-- ___
--  2024-03-18 10:07
--  All the following versions of code do the same thing

--    return :: Monad a => a -> m a

--    main = do x <- getLine
--              y <- getLine
--             return (x ++ y)

--    foo = getLine      (a plan that hasn't been executed; only the main plan )
--    main = do x <- getLine
--              y <- getLine
--             return (x ++ y)

--    foo = getLine
--    main = do x <- foo
--              y <- getLine
--             return (x ++ y)

--    foo = getLine
--    main = do x <- foo
--           y <- foo
--           return (x ++ y)
--  better way to write (using applicative):  main = (++) <$> getLine <> getLine

-- ___
--  2024-03-20 10:03
--    instance Monad Maybe where

--      do x <- foo
--         something

--       same as foo >>= x -> something

--      (>>=) Nothing something = Nothing
--      (>>=) Just x something = something x

--  a simple variable calculator
--    data Exp v = Var v                                     (syntactic Monad)
--               | Const Int
--               | Plus (Exp v) (Exp v)
--               | Mul (Exp v) (Exp v)

--    instance Functor Exp where

--    --fmap :: (v1 -> v2) -> Exp v1 -> Exp v2
--    fmap vt (Var x) = Var (vt x)                      (vt - variable translation)
--    fmap vt (Plus e1 e2) = Plus (fmap vt e1) (fmap vt e2)
--    fmap vt (Mul e1 e2) = Mul (fmap vt e1) (fmap vt e2)
--    fmap _ c = c

--  join :: Monad m => m (m a) -> m a
--  join mma = do ma <- mma
--                   ma
--  (>>=) :: Monad m => (a -> mb) -> mb
--     (>>=) m f = join (fmap f ma)

--  so Monads instances can be defined either by defining join or bind

--       join :: Exp (Exp v) -> Exp v        more natural to define join for expressions

--       join (Const i) = Const i
--       join (Var e) = e
--       join (Plus e1 e2) = Plus (join e1) (join e2)
--       join (Mul e1 e2) = Mul (join e1) (join e2)

-- ___
--  2024-03-22 10:01
--  List Monad
--    do x <- alist
--       foo x
--    equivalently     alist >>= foo

--  alist :: [a]         foo :: a -> [b]
--     read the do statement as for each element in the list do foo x and concatenate (collect) the result
--  alist >>= foo = concat $ map foo alist

-- ___
-- 2024-03-27 10:18:20

--  Parsing Method
-- Build a library for constructing parser

-- newtype Parser a = Parser (String -> a)

-- Parser Combinator libraries
-- Parsec, mega parsec, atto parsec

-- Have to parse the rest of the input and give parse error
-- Result a - captures this idea

-- String -> Maybe (a,String) is more apt

newtype Parser a = Parser (String -> Maybe (a, String))

-- not good enough error msg. It just returns Nothing if

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser fn
  where
    -- fn :: String -> Maybe (Char, String)
    fn (x : xs) =
      if pr x
        then Just (x, xs)
        else Nothing
    fn [] = Nothing

-- digit = satisfy isDigit
-- char :: Char -> Parser Char
-- char x = satisfy ( \c -> c = x)

-- type Result a = Maybe(a,String)

-- runParser :: Parser a -> String -> Result a
-- runParser (Parser fn) input = fn input
-- or better:
runParser (Parser fn) = fn

-- (<|>) :: Parser a -> Parser a -> Parser a         (like or: try out the first one. If it fails try the second one)

-- backtracking happens as it checks again on input (Recursive descent parsing)

p1 <|> p2 = Parser fn
  where
    fn input = case runParser p1 input of
      Nothing -> runParser p2 input
      x -> x

-- many :: Parser a -> Parser [a]          it should parse regex a
-- Do it urself

data Result a = Ok a String | Err

instance Functor Result where
  fmap f (Ok a s) = Ok (f a) s
  fmap _ Err = Err

-- instance Functor Parser where
--   fmap :: (a->b) -> Parser a -> Parser b

--   fmap f pa = Parser fn
--     where -- fn :: String -> Result b
--       fn input = fmap f (runParser pa input)

-- 2024-04-01 21:35:10
-- instance Applicative Parser where
--   pure :: a-> Parser a
--   pure a = Parser (pfn :: String -> Result a)
--     where pfn str = Ok a str
-- pure parsing, without even looking at a -> always succeeds

-- (<*>) :: Parser (a->b) -> Parser a -> Parser b

-- pf <*> pa = Parser (pfnb :: String -> Result b)
--   where pfnb str = case runParser pf str of
--     Ok f rest -> fmap f (runParser pa rest)
--     Err -> Err

-- an application of above

data Person = Person String Int

-- Person datatype with Name and age

-- digit = satisfy isDigit
-- alpha = satisfy isAlpha

-- many :: Parser a -> Parser [a]        -- regex: a*
-- many1 :: Parser a -> Parser [a]       -- regex: a+

-- name = many1 alpha
-- age = read <$> (many1 digit)

-- newPerson = Person <$> name <*> age

-- -- string and list of char are the same

-- many1 p = (:) <$> p <*> manyp
-- manyp p = many1 p <|> pure []

-- --elegant mutually recursive definitions
-- -- base case comes from the parser of p in many1