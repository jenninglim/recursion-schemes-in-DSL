# Scheming it all.
## Recursion Scheme in Domain Specific Languages.
## Abstract
In this paper we will introduce structure recursion as a way to derive program
semantics.
{Add more here}

## Introduction
Abstraction has proved to be one of the most influencial and ubiquitous themes
in computer science. It is unsurprising that one of its most notable triumphs has been its 
significance in the design of programming languages. The most successful will 
provide various techniques for abstraction at the software level such as 
higher-order functions, objects etc.

General Purpose Languages ("GPL") are programming languages designed to tackle 
problems in every problem domain. Their generality pushes programmers to adopt
them in their repetoire, however this makes them convoluted to anyone lacking
programming expertise. This motivates the idea of raising the abstraction
level such that languages are designed for a particular problem domain:
it is _domain specific_.

Domain Specific Languages ("DSL") are, usually declarative, programming languages that
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
features of the base language thus the embedded DSL offer addition power of its base GPL
as well as their domain specific expressive power. Embedded DSLs
often extends functional languages - features part of this class of languages
such as higher order functions, monads, algebraic data types makes the develop 
of embedded DSL much easier.

The nature of functional languages has meant that its programs are _pure_.
With no state or side-effects, many computation are naturally
expressed as recursive functions. Unsurprisingly, many of which share the same recursive patterns
which can be abstracted away. An example that many functional programmers will be familiar
with is  _fold_ as a stardard recursive operator, it captures the common pattern of
traversing and processing a structurally inductive data structure. The abundance of recursion does not
end there, in fact, the denotational semantics, an approach that gives mathematical models to the semantics
of a program, can be structured and characterised by folding over its syntax[1]. This is why we can fold 
DSL with great success [2].

This motivates us to look closely at the generalisations of folds and unfolds as a set of
combinators introduced by Meijer et al called _recursion schemes_. They uses the abstract nonsense 
of category theory to structure the traversing and evaluation of inductive data
structures.

The structure of the report is as follows:

1. A brief introduction to Category Theory - Many ideas in Haskell have originated from
   category theory and arguably the reason why functional programming is so succesful.
   This section introduces the necessary knowledge for understanding the report.
2. Explicit and Structure Recursion - We will begin by discussing the drawbacks of explicit 
   recursion and  why we should always use structured programming if possible.
3. Pattern Functors and Fix-point of functors - Given a recursive data structure we will
   show how to abstract away recursion at the type level.
4. Catamorphims - The generalised fold. We will discuss its implementation and present 
   useful theorems.

## 0. Introduction to Category Theory
Category Theory has proved to be indepensible in the development of functional
programming. The nature of functional programming has meant that its programs
consists of the idea of chaining functions together. This is exactly the 
structure that category theory captures. Haskell types and functions can be
modelled as a _category_, where the types form the objects and functions between
two types, the morphism. 

But what is a category?  
A category C is an algebraic structure defined on a collection of:

 * objects (denoted A, B, C, ...)
 * morphism between a pair A,B denoted C(A,B)
 
additionally, they must satsify:

 * for each object A, there is an identity morphism.
 * the morphisms are associative.
 * for every object a,bc a binary operation called the _composition of morphisms_.
 
Morphisms can be thought of as special functions between objects that perserves
its structure satisfying associativity, composition and the existence of an
identity for every object. Because of this, many category theorists believe that
the morphisms are of greater importance than objects  because they reveal the
true underlying structure.

It is natural to consider a structure preserving map similar idea to morphism but
for categories. The functor is a mapping between categories but with additional
properties so that the categorical structure is preserved.  
It is formally, a Functor F : C -> D consists of:

 * mapping A-> FA: C -> D
 * mapping f -> F f: C(A,B) -> D(FA, FB).
 
such that:

 * F id = id
 * F(g.f) = F g .F f

These additional laws respect the nature of a morphism in the category by
preserving the identity and the composition of morphisms.  

An endofunctor is an functor from a category to itself. 
In Haskell, the definition of a functor corresponds with the categorical
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

Given a category C and an endofunctor F: C -> C then an F-algebra is a tuple
(A,f) where A is an object in C and f is a morphism F(A) -> A. The object A
is called the _carrier_ of the algebra.

In Haskell, the definition of an F-Algebra is found in `Control.Functor.Algebra`

```
  type Algebra f = f a -> a
```

{Expand on Algebras}
{Introduce ideas of initiality}

## 1. Explicit and Structured Recursion

Recursion in its essence is something defined in terms of itself.
It is a simple yet powerful concept that forms the bread and
butter for functional computation. Explicit recursion is a way of
describing recursion that is overused for the uninitiated. 
Arbitrary properties of the function  will need to be written and 
proved over and over again which can be simply avoided by carefully
abstracting away common recursive patterns.

Its profuseness implies that abstracting away common patterns could
replace a plethora of explicit recursive functions. Meijer et al
introduced a set of recursive operators that models different types
of recursion. The catamorphism models iteration which is a special
case of primitive recursion which is modelled by the paramorphism.
Meijer also introduced its duals for unfolds and corecursion,
anamorphism and apomorphism but they are outside the scope of the essay.

We have known for a long time the use of `gotos` in imperative programming
obscures the structure of the program and reduces the programmers ability to 
reason with our code. For the same reason `gotos` should be avoided, we
should always use structured recursion whenever possible. This is because
although explicit recursion is more intuitive, structural recursion provides
a way to reason with our code like never before. They provide us with a catalogues
of useful theorems and properties what we can infer in our functions for free.
Additionally, as a byproduct of abstracting away the format of traversals, we
have separated how the function is computed rather than its underlying purpose.
This means for programmers, trained in the art of structuring recursion, 
can concentrate on the what the computation is doing rather than how.

## 2. Hiding (explicit) recursion

In Section 2, we have detailed the differences between explicit and structured
recursion and explained that whenever there is a choice between structured
and explicit recursion, structured recursion should always be used.

### 2.1 Parameterising Recursion

Let’s consider a very simple language of addition and subtraction.

```
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
```

We can have parameterised the recursion, by marking the recursive spot `x`:

```
data ExprF x = Val Int
             | Add x x
             | Sub x x
```
 
In the new definition of `Expr`, `ExprF`,
we have parameterised this type in terms of its subexpression,
this is called the _pattern functor_ which is almost identical to the
original `Expr`.

It is trivial to make ExprF an instance of `functor`.

```
instance Functor ExprF where
  fmap :: (a -> b) -> f a -> f b
  fmap f (Val Int) = Int
  fmap f (Add x y) = Add (f x) (f x)
  fmap f (Sub x y) = Sub (f x) (f y)
```

In fact, it is so trivial that GHC can derive it for us if we 
enable the following extension, called a language pragma:

```
{-# LANGUAGE DeriveFunctor #-}
```
and we can just use `Deriving Functor` in our data declaration.

However, `ExprF` is not quite equivalent, it need to
somehow arbitrarily nest `ExprF` in the definition.

### 2.2 Fix Point of Functors

In lambda calculus, it is not possible to refer to the function definition in its body;
there is no feature for (explicit) recursion. However, by using the paradoxical Y 
combinator, we can replicate recursive behaviour. It is, by definition, a higher-order 
function, f, that takes a non-recursive function that satisfies the following:

$$y f = f (y  f) \forall f$$

This concept can be defined in Haskell's type definition as follows: 

```
Fix f = In (f (Fix f))
```

By using `Fix`, we can define our corresponding pattern functor
in such a way, called the fixed point of functors, which is
isomorphic to the original definition,

Fix ExprF ~ Expr

This technique of redefining recursive data types is very powerful.
Interestingly [4], the fixed point of functors corresponds to the 
initial algebra.  
BOOM.

## Catamorphism
Catamorphism are generalisations of folds, it replicates the concept of 
iterative functions by destroying the data structure while traversing it.

{Concrete Example}

{Theorems}
{Fusion}
{cata compose}
{Banana Split Theorem}

## References
[1] Fold and Unfold for Program Semantics.  
[2] Folding DSL: Deep and Shallow Embedding.  
[3] Functional programming with bananas, lenses, envelopes and barbed wire.  
[4] Recursive types for free.
