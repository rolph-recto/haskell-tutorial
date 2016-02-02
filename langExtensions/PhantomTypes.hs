-- PhantomTypes.hs
-- Phantom types are type parameters that are not constrained by the values
-- of the types' inhabitants (i.e., parameter does not occur in constructors)

-- add lang extension so we can create typeclass instances for string
-- why do we need to use this? String is just a type synonym for [Char],
-- (i.e., [] Char) and typeclass instances have to be of the form
-- T x1 x2 ... xn where xi are type variables. Since Char is not a variable,
-- We can't
--
-- This is to prevent cases where instance [a] ... and instance [Char] are
-- declared, and GHC can't figure out which instance to use.
-- FlexibleInstances lifts this restriction; OverlappingInstances picks
-- the most specific instance -- which in this case is [Char].
-- TypeSynonymInstances allows one to define instances for String instead
-- of [Char]
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char (toLower)

class Cleanable a where
  clean :: a -> a

data Valid
data Invalid

data FormInfo a b = FormInfo a

createForm :: Cleanable a => a -> FormInfo a Invalid
createForm x = FormInfo x

combineFormData :: Monoid a => FormInfo a b -> FormInfo a b -> FormInfo a b
combineFormData (FormInfo x) (FormInfo y) = FormInfo (x `mappend` y)

validate :: Cleanable a => FormInfo a Invalid -> FormInfo a Valid
validate (FormInfo x) = FormInfo (clean x)

unpack :: FormInfo a Valid -> a
unpack (FormInfo x) = x

instance Cleanable String where
  clean = map toLower

main = do
  let f = createForm "OK THIS IS A TEST"
  let g = createForm " OK THIS IS ANOTHER FORM"
  let h = combineFormData f g
  print $ unpack $ validate h
  -- the following should result in a type error
  -- print $ unpack h

