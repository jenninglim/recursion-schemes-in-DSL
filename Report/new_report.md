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
languages implies that they trade their generality for focused expressivity.
This often makes DSLs small languages since only the essential features are
captured, examples of DSLs include SQL, HTML, CSS etc.

There are two main ways of developing DSLs:

* Standalone.
* Embedded.

Standalone is a classical approach for implementing a new language. It involves
implementing the language from scratch: everything that you need in a language
has to be developed, no concessions are made. The characteristics of this language
is tailored specifically to the problem domain. However, the drawback of this 
approach makes it very costly, as a result standalone DSLs are rarely developed.

Embedded DSLs are implemented by extending a GPL, this approach uses the
existing language constructs to build the language. They share the generic 
features of the base language thus the embedded DSL offer their domain specific
expressive power as well as the additional power of its base GPL. Embedded DSLs
often extends functional languages - features part of this class of languages
such as higher order functions, monads, algebraic data types makes the develop 
of embedded DSL much easier.

{Semantics as folds}
The nature of functional languages has meant that its programs are referentially
transparent or _pure_. With no state or side-effects, many computation are naturally
expressed as recursive functions. Unsurprisingly, many share the same recursive patterns
which can be abstracted away. An example that many functional programmers will familiar
with is  _fold_ as a stardard recursive operator, it captures the common pattern of
traversing and processing a list. The abundance of recursion does not
end there, in fact, the denotational semantics of a program can be structured and 
characterised by folding over its syntax[1]. This is why we can fold DSL with
great success [2].

{Recursion schemes}
This motivates us to look at the generalisations of folds and unfolds as a set of
combinators, called _Recursion Schemes_. Meijer et al uses the abstract nonsense 
of category theory to structure the traversing and evaluation of inductive data
structures.

{Structure}
The structure of the report is as follows:

1. A brief introduction to Category Theory - Many ideas in Haskell have originated from
   category theory and arguably the reason why functional programming is so succesful.
   This section is an optional read and purely for interest.
2. Explicit and Structure Recursion - We will begin by discussing the drawbacks of explicit 
   recursion and  why we should always use structured programming if possible.
3. Pattern Functors and Fix-point of functors - Given a recursive data structure we will
   show how to abstract away recursion at the type level.
4. Catamorphims - The generalised fold. We will discuss its implementation and present 
   useful theorems.

## 0. Introduction to Category Theory
{Motivation}
Category Theory has proved to be indepensible in the development of functional
programming. The nature of functional programming has meant that its programs
consists of the idea of chaining functions together. This is exactly the 
structure that category theory captures. Haskell types and functions can be
modelled as a _category_, where the types form the objects and functions between
two types, the morphism.

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


## 1. Explicit and Structured Recursion
{Explicit Recursion}

{Structured Recursion}

{Advantages of structuring recursion}

## 2. Hiding (explicit) recursion

In Section 2, we have detailed the differences between explicit and structured
recursion, and explained why structure recursion should always be used.

### 2.1 Parameterising Recursion

Let’s consider a very simple language of addition and subtraction.

```
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
```

We can have parameterised the recursion, by marking the recursive spot x:

```
data ExprF x = Val Int
             | Add x x
             | Sub x x
```
 
In the new definition of `Expr`, `ExprF`,
we have parameterised this type in terms of its subexpression,
this is called the _pattern functor_ which is almost identical to the
original `Expr`. However, `ExprF` is not quite equivalent, we need to
somehow arbitrarily nest `ExprF` in the definition.

### 2.2 Fix Point of Functors

{Brief Explaination}

## Catamorphism
{Definition}

{Concrete Example}

{Theorems}

## References
[1] Fold and Unfold for Program Semantics.
[2] Folding DSL: Deep and Shallow Embedding.
[3] Functional programming with bananas, lenses, envelopes and barbed wire.
