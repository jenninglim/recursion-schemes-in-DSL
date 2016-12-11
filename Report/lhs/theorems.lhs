> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE UndecidableInstances #-}

> import Text.PrettyPrint.Leijen

> newtype Fix f = In { out :: f (Fix f) } 

Controversal but convinient.

> deriving instance (Show (f (Fix f))) => Show (Fix f)

F-algebra

> type Algebra f a = f a -> a

Catamorphism

> cata :: (Functor f) => Algebra f a -> Fix f -> a
> cata alg = alg . fmap (cata alg) . out

Our simple language. The monoid of integers with a binary op.

> data ExprF k = Val Int
>              | Add k k
>              deriving (Show, Functor)

> type Expr = Fix ExprF

f :: f a -> a
g :: f b -> b
h :: a -> b

h . f = g . fmap h => h . cata f = cata g

Simple fusion, pretty printer.
f :: ExprF String -> String
g :: ExprF Doc -> Doc
h :: Doc -> Doc

> prettyFast :: Expr -> Doc
> prettyFast = cata alg
>   where alg :: ExprF Doc -> Doc
>         alg (Val x)   = text $ show x
>         alg (Add x y) = parens $ x <+> text "+" <+> y

> prettySlow :: Expr -> Doc
> prettySlow = text . cata alg
>  where alg :: ExprF String -> String
>        alg (Val x) = show x
>        alg (Add x y) = x ++ "+" ++ y

Example: Compose Law.

The catamorphism compose law \[4\] states that:

     cata f . cata (Fix . h) = cata (f . h)

where,

     f :: f a -> a
     h :: g a -> f a

UH OH



Example: Banana-split theorem.

> split :: (a -> b) -> (a -> c) -> (a -> (b, c))
> split f g = \x -> (f x, g x)

Banana-split theorem states that:

split (cata f) (cata g) = cata (split (f . fmap2 id fst) (g . fmap2 id snd))


