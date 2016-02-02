{-# LANGUAGE ExistentialQuantification #-}

-- we use existential types to create "object-oriented" style constructions

import Control.Monad

-- typeclasses are basically interfaces; while they cannot possess state,
-- it can provide specifications that a class (which is an ADT) must
-- implement
class AnimalClass a where
  -- object attributes / methods take in a "self" parameter
  _species :: a -> String
  _name :: a -> String
  _talk :: a -> String

-- wrapper type for types that implement AnimalClass
data Animal = forall a. AnimalClass a => Animal a

-- functions that unwrap the existential type
species :: Animal -> String
species (Animal a) = _species a

name :: Animal -> String
name (Animal a) = _name a

talk :: Animal -> String
talk (Animal a) = _talk a


-- subclassing: use typeclass restrictions on typeclass definitions!
class AnimalClass a => MammalClass a where
  _weaned :: a -> Bool

data Mammal = forall a. MammalClass a => Mammal a

weaned :: Mammal -> Bool
weaned (Mammal m) = _weaned m


-- subclasses are actual datatypes
data Cow = Cow {
  cowName :: String,
  cowAge :: Int,
  hasMilk :: Bool
}

instance AnimalClass Cow where
  _species _           = "cow"
  _name (Cow name _ _)   = name
  _talk (Cow name age _) = "Moo! I'm " ++ name ++ ". I am " ++ show age ++ " years old."
  
instance MammalClass Cow where
  _weaned (Cow _ age _) = age < 1


data Pig = Pig {
  pigName :: String,
  pigAge :: Int
}

instance AnimalClass Pig where
  _species _           = "pig"
  _name (Pig name _)   = name
  _talk (Pig name age) = "Oink! I'm " ++ name ++ ". I am " ++ show age ++ " years old."
  
instance MammalClass Pig where
  _weaned (Pig _ age) = age < 1


main = do
  let bob = Cow "bob" 10 False
  let guy = Pig "guy" 20

  -- casted list of animals
  let alist = [Animal bob, Animal guy]
  forM_ alist (print . talk)

  -- casted list of mammals
  let mlist = [Mammal bob, Mammal guy]
  forM_ mlist (print . weaned)

