# Scheming a Problem
## Recursion Scheme in Domain Specific Languages.
## Abstract
In this paper we will introduce structure recursion as a way to describe
and reason with program semantics.

## Introduction
Abstraction has proved to be one of the most influencial and ubiquitous themes
in computer science. It is perhaps unsurprising that its most notable success
has been its occurrence in programming languages, the most sucessful must 
provide various techniques such as higher-order functions, objects etc. for
abstraction at the software level.

{Introduce DSL}

{Embedded DSLs in depth.}

{Semantics as recursion}

{Recursion schemes}

{Structure}

## Introduction to Category Theory
{Motivation}

{Category}
But what is a category?  
It is an algebraic structure defined on a collection of:
 * objects
 * morphisms
 
Morphisms can be thought of as special functions between objects that perserves
its structure satisfying associativity, composition and the existence of an
identity for each object. Because of this, many category theorists believe that
the morphisms are of greater significance than objects  because they reveal their
true underlying structure.

{Functor}
It is natural to consider a structure preserving map similar idea to morphism but
for categories, this is the concept behind a functor. It is formally, F : C -> D
consists of:
 * mapping A-> FA: C -> D
 * mapping f -> F f: C(A,B) -> D(FA, FB).
such that:
 * F id = id
 * F(g.f) = F g .F f
An endofunctor is an functor from a category to itself.

In Haskell, the definition of a functor is reflected with the categorical
endofunctor, defined as follows:
 * 




{Algebra}

## Explicit Recursion
Why is structured recursion bad?

## Parameterising Recursion

## Fix Point of Functors

## Catamorphism

## Paramorphism

## References



