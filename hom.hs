-- Objects in Category C
type X = Int
type A = Int
type B = Int

-- Newtype to represent the functor F's action on objects of C
newtype F a = F a deriving (Show)

-- Functor instance for F (F: C -> Set)
instance Functor F where
    -- fmap for F corresponds to F acting on arrows in C
    fmap :: (a -> b) -> F a -> F b
    fmap f (F x) = F (f x)

-- Arrow ha: Homc(X,A)
ha :: X -> A
ha x = x + 1

-- Arrow g: A->B
g :: A -> B
g x = x + 1

-- Arrow hg: Homc(X,A)->Homc(X,B)
hg :: (X -> A) -> (X -> B)
hg a x = g (a x)

-- Specific element s in F(X), or "starting point" 
s :: F X
s = F 5

-- Natural transformation component for the functor F at object A
-- η_A: Hom(X, A) -> F(A)
etaA :: (X -> A) -> F A
etaA f = fmap f s
