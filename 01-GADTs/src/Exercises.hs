{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Exercises where





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountableNil :: CountableList  -- ...
  CountableCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountableNil = 0
countList (CountableCons hd tail) = count hd + countList tail


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountableNil = CountableNil
dropZero (CountableCons hd tail) =
  if count hd == 0 then dropZero tail else (CountableCons hd tail)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList
  -- ...

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil = AnyNil
reverseAnyList l@(AnyCons x AnyNil) = l
reverseAnyList (AnyCons x xs) = go xs (AnyCons x AnyNil)
  where
  go AnyNil xs = xs
  go (AnyCons y ys) xs = go ys (AnyCons y xs)

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

lengthAnyList :: AnyList -> Int
lengthAnyList AnyNil = 0
lengthAnyList (AnyCons _ xs) = 1 + lengthAnyList xs

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _ = False

instance Show AnyList where
  show = error "What about me?"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Eq output => Eq (TransformableTo output) where
  (==) (TransformWith f1 i1) (TransformWith f2 i2) = f1 i1 == f2 i2

instance Functor TransformableTo where
  fmap f (TransformWith g inp) = TransformWith (f . g) inp




{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?
areEqual :: EqPair -> Bool
areEqual (EqPair x y) = x == y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?





{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox s intBox) = getInt intBox

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ _) = 1
countLayers (StringBox _ _) = 2
countLayers (BoolBox _ _) = 2

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- peelLayer :: MysteryBox a -> MysteryBox b
-- peelLayer (IntBox _ emptyBox) = emptyBox
-- peelLayer (StringBox _ emptyBox) = emptyBox




{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!
head' :: HList (hd, tail) -> hd
head' (HCons hd _) = hd

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- appendHList :: HList a -> HList b -> HList c
-- appendHList HNil x = x




{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: l -> c -> r -> HTree (Branch l c r)
  -- ...

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?
delLeftTree :: HTree (Branch l c r) -> HTree (Branch (HTree Empty) c r)
delLeftTree (HBranch l c r) = HBranch HEmpty c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!
instance Eq (HTree Empty) where
  (==) _ _ = True

instance (Eq l, Eq c, Eq r) => Eq (HTree (Branch l c r)) where
  (==) (HBranch l1 c1 r1) (HBranch l2 c2 r2) = l1 == l2 && c1 == c2 && r1 == r2


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList b a
  ACons :: a -> AlternatingList b a -> AlternatingList a b
  -- ...

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ANil = []
getFirsts xs@(ACons _ _) = goTrue [] xs
  where
  goTrue :: [a] -> AlternatingList a b -> [a]
  goTrue xs (ACons a als) =
    goFalse
      (a : xs)
      als
  goTrue xs ANil = xs
  goFalse xs (ACons a als) =
    goTrue
      xs
      als
  goFalse xs ANil = xs

getSeconds :: AlternatingList a b -> [b]
getSeconds ANil = []
getSeconds xs@(ACons _ _) = goFalse [] xs
  where
  goTrue :: [a] -> AlternatingList a b -> [a]
  goTrue xs (ACons a als) =
    goFalse
      (a : xs)
      als
  goTrue xs ANil = xs
  goFalse xs (ACons a als) =
    goTrue
      xs
      als
  goFalse xs ANil = xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues ANil = (mempty, mempty)
foldValues xs@(ACons _ _) = goTrue (mempty, mempty) xs
  where
  goTrue (ma, mb) (ACons a als) =
    goFalse
      ((a <> ma), mb)
      als
  goTrue xs ANil = xs
  goFalse (ma, mb) (ACons a als) =
    goTrue
      (ma, a <> mb)
      als
  goFalse xs ANil = xs





{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals x y) = eval x == eval y
eval (Add x y) = eval x + eval y
eval (If x y z) = if eval x  then eval y else eval z
eval (IntValue x) = x
eval (BoolValue x) = x

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

data Typed where
  IntType :: Expr Int -> Typed
  BoolType :: Expr Bool -> Typed

tidy :: DirtyExpr -> Maybe Typed
tidy (DirtyEquals x y) = case (tidy x, tidy y) of
  (Just (IntType x), Just (IntType y)) -> Just (BoolType (Equals x y))
  _                                    -> Nothing

tidy (DirtyAdd x y) = case (tidy x, tidy y) of
  (Just (IntType x), Just (IntType y)) -> Just (IntType (Add x y))
  _                                    -> Nothing

tidy (DirtyIf p t f) = case (tidy p, tidy t, tidy f) of
  (Just (BoolType p'), Just (IntType t'), Just (IntType f')) ->
    Just (IntType (If p' t' f'))
  (Just (BoolType p'), Just (BoolType t'), Just (BoolType f')) ->
    Just (BoolType (If p' t' f'))

tidy (DirtyIntValue  x) = Just (IntType  (IntValue  x))
tidy (DirtyBoolValue x) = Just (BoolType (BoolValue x))

parse :: DirtyExpr -> Maybe (Expr Int)
parse dexp = case tidy dexp of
  (Just (IntType expr)) -> Just expr
  _ -> Nothing
-- parse' :: DirtyExpr -> Maybe (Expr Int)
-- parse' = \case
--   DirtyEquals _ _ -> Nothing
--   DirtyAdd lhsExpr rhsExpr ->
--     case (parse lhsExpr,parse rhsExpr) of
--       (Just (IntValue i1), Just (IntValue i2)) ->
--         Just (Add (IntValue i1) (IntValue i2))
--       _  -> Nothing
--   DirtyIf condExpr thenExpr elseExpr ->
--     case (parse condExpr, parse thenExpr, parse elseExpr) of
--       (Just (BoolValue b1), Just (IntValue i1), Just (IntValue i2)) ->
--         Just (If (BoolValue b1) (IntValue i1) (IntValue i2))
--       _  -> Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  NilTAL :: TypeAlignedList a a
  ConsTAL :: (c -> b) -> TypeAlignedList b a -> TypeAlignedList c a
  -- ...

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs NilTAL x = x
composeTALs x NilTAL = x
composeTALs gs (ConsTAL f fs) = ConsTAL f (composeTALs gs fs)

