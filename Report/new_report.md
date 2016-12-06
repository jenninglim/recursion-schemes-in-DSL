# Scheming a Problem
## Recursion Scheme in Domain Specific Languages.
## Abstract
In this paper we will introduce structure recursion.  
{Add more here}

## Introduction
Abstraction has proved to be one of the most influencial and ubiquitous themes
in computer science. It is unsurprising that its most notable triumph has been its 
significance in the design of programming languages: the most sucessful must 
provide various techniques for abstraction at the software level such as 
higher-order functions, objects etc.

{Introduce DSL}

{Embedded DSLs in depth.}

{Semantics as recursion}

{Recursion schemes}

{Structure}

## Introduction to Category Theory
{Motivation}

But what is a category?  
It is an algebraic structure defined on a collection of:
 * objects
 * morphisms
Morphisms can be thought of as special functions between objects that perserves
its structure satisfying associativity, composition and the existence of an
identity for every object. Because of this, many category theorists believe that
the morphisms are of greater importance than objects  because they reveal the
true underlying structure.

It is natural to consider a structure preserving map similar idea to morphism but
for categories, this is the concept behind a functor. It is formally, a Functor
F : C -> D consists of:
 * mapping A-> FA: C -> D
 * mapping f -> F f: C(A,B) -> D(FA, FB).
such that:
 * F id = id
 * F(g.f) = F g .F f
These additional laws respect the nature of a morphism in the category by
perserving the identity and the composition of morphisms.
An endofunctor is an functor from a category to itself.

In Haskell, the definition of a functor is reflected with the categorical
endofunctor, defined as follows:
```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
The additional properties that a categoric functor must satisfied, is captured in
haskell by the functor laws, that each an every instance must satisfy:
```
    fmap id = id
    fmap (f . g) = fmap f . fmap g
```

{Algebra}

## Explicit Recursion
Why is structured recursion bad?

## Parameterising Recursion

## Fix Point of Functors

## Catamorphism

## Paramorphism

## References



