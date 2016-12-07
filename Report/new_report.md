# Scheming a Problem
## Recursion Scheme in Domain Specific Languages.
## Abstract
In this paper we will introduce structure recursion as a way to derive program
semantics.
{Add more here}

## Introduction
Abstraction has proved to be one of the most influencial and ubiquitous themes
in computer science. It is unsurprising that its most notable triumph has been its 
significance in the design of programming languages: the most sucessful will 
provide various techniques for abstraction at the software level such as 
higher-order functions, objects etc.

{Introduce DSL}
General Purpose Languages ("GPL") are programming languages designed to tackle 
problems in every problem domain. Their generality pushes programmers to adopt
them in their repetoire, however this makes them convoluted to anyone lacking
programming expertise. This motivates the idea of raising the abstraction
level such that languages are designed for a particular problem domain:
it is _domain specific_.

{Embedded DSLs in depth.}
Domain Specific Languages are, usually declarative, programming languages that
offer more expressivity over a specific problem domain. Their development involves
a close analysis of the problem domain such that the entire semantics of the
problem domain should be captured - no more and no less. The nature of these
languages implies that they trade their generality for focussed expressivity.
Their nature often makes DSLs small languages since only the essential features is
captured, examples of DSLs include SQL, HTML, CSS etc.

There are two main ways of developing DSLs:

* Standalone.
* Embedded.

Standalone is a classical approach for implementing a new language. It involves
implementing the language from scratch, everything that you need in a language
has to be developed, no concessions are made, everything about this language
is tailored specifically to the problem domain. However, the nature of the 
approach makes it very costly thus standalone DSLs are rarely developed.

Embedding DSLs can be implemented by extending a GPL, this approach uses the
existing language constructs to build the language. They share the generic 
features of the base language thus the embedded DSL offer their domain specific
expressive power as well as the additional power of its base GPL. Embedded DSLs
often extends functional languages - the features part of this class of languages
makes developing embedded DSL easier.

{Semantics as folds}


{Recursion schemes}

{Structure}

## Introduction to Category Theory
{Motivation}

But what is a category?  
It is an algebraic structure defined on a collection of:

 * objects.
 * morphisms.
 
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
Haskell by the functor laws, that each and every instance must satisfy:
```
  fmap id = id
  fmap (f . g) = fmap f . fmap g
```

{Algebra}

## Explicit vs Structured
{Explicit Recursion}

{Structured Recursion}

{Advantages of structuring recursion}

## Parameterising Recursion

## Fix Point of Functors
{Brief Explaination}

## Catamorphism
{Definition}

{Concrete Example}

{Theorems}

## References



