# Using Recursion schemes in Domain Specific Language.

## Introduction

The most successful computer systems can stand the test of time;
it requires numerous people to work together to test, maintain and update it throughout its lifespan.
And abstraction,
which is one of the most ubiquitous themes in computer science,
provides a good way to manage its complexity and guarantee its progression.
Thus it comes as no surprise that modern programming languages depend an and exploit this.
Language engineers have introduced a variety of abstraction techniques,
each of which has encompassed the idea of generality.  

Although generality is a good thing,
a generic approach that provides solutions in multiple problem domains could mean that the solution is not an optimal one,
that their very generality could count against them.
This motivates the idea of programming languages designed specifically for a certain application,
that is,
it should capture the entirety of the semantics of the problem domain:
it is domain specific.

Domain specific languages (DSL) are often small languages that trade their generality for more expressivity (than general purpose language) over a specific domain and they are also often easier to use.

There are two main ways of implementing a DSL: 

* Standalone: This is the classical approach; it involves implementing the domain specific language from scratch.

  {expand here}

* Embedded languages: An existing general purpose programming language with all its constructs is used as a “based” to build a domain specific language.

  An embedding programming language offer their expressive domain specific power as well as the additional power of its base general purpose language.

There are two main approaches to implementing an embedded DSL:

* Shallow: Its semantics are implemented directly and so they are represented as functions. 
* Deep: It is implemented by constructing an abstract syntax tree that represents the expression tree.

In shallow embedding the semantics depend directly on the semantics of the composed term: it only captures compositional semantics.
In contrast,
it is possible to define a complex interpretation,
that is non-compositional,
of deep embedding that cannot be represented in shallow embedding. Even though this is possible, Gibbons et al say that they are equivalent.

It is extremely common to have nested data structures in a specific problem domain,
so it is trivial to define an equivalent data representation in the DSL.
However,
to traverse such a structure, we require a recursive function.
Because of their abundance,
programmers often do not notice that they are using recursive functions so the idea of generalising traversals could replace a large amount of type specific functions that utilises recursion.

Meijer et al uses simple yet powerful abstract nonsense in category theory to introduced the idea of recursion schemes,
a set of combinators that formulates the process of traversing and evaluating sophisticated inductive data structures.[3][1].
This generalisation allows us to structure our program in a well-defined way by decoupling how a function recurse over a data structure from the underlying purpose of the function meaning,
this means programmers can just focus on what the function is actually does.

The authors begin to criticising those who functionally program without recursion schemes, comparing them to imperative programmers who use gotos.
In Djikstra’s letter to the ACM where he expressed his concerns that goto masks the flow of the program; here the use of unstructured (explicit) recursion obscures the underlying algorithmic structure, making it hard to prove arbitrary properties of the function, the analogy here is appropriate.

The purpose of this essay is to introduce the idea of recursion schemes, this will be done by introducing a simple language….

## Parameterising Recursion
Let’s consider a very simple language of addition and subtraction.

```
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
```

We can have parameterised the recursion, by marking the recursive spot x:

```
data ExprF x = Val Int
             | Add x x
             | Sub x x
```
 
In the new definition of `Expr`, `ExprF`,
we have parameterised this type in terms of its subexpression called the pattern functor which is almost identical to the original `Expr`.

This technique of redefining inductive data types allows us to derive Functor for free,
it also been used to great effect to write a set of generalized recursion schemes.
However,
the renewed of `Expr` is not the equivalent as the original `Expr`,
it needs to arbitrarily nest `ExprF`.
How can we proceed?

## Fixed Points
In lambda calculus, it is not possible to refer to the function definition in its body; there is no feature for (explicit) recursion.
However,
by using Church’s Y combinator,
we can replicate a recursive function.  
It is, by definition, a higher-order function that takes a non-recursive function that satisfies the following:

$$Y f = f (y  f) \forall f$$

This is an idea can be transferred into Haskell to help “fix” our pattern functor by describing the Y combinator in Haskell’s type system, we have:

```
Fix f = In (f (Fix f))
```

We can now describe our `ExprF` in such a way so that it is isomorphic to `Expr`. We can redefine `eval` for `ExprF`:

```
eval :: Fix ExprF -> Int
eval (In f)
```
## Enter the Catamorphism
Meijer et al introduced the idea of the generalised fold called a catamorphism (from the greek preposition for destruction).
In Category Theory, it denotes the unique homomorphism from an initial algebra into some other algebra.

\pagebreak

## Reference
1. Shaw, Mary. Abstraction Techniques in modern programming languages. (1984). IEEE Software. Pages 10-26.
2. Van Deursen, Arie. Klint, Paul. Visser, Joost. Domain-Specific Languages: An Annotated Bibliography. (2000). ACM SIGPLAN Notices.
3. [1] Meijer, Erik. Fokkinga, Maarten. Paterson, Ross. Functional programming with bananas, lenses, envelopes and barbed wire. (1991). FPLCA. LNCS, vol. 523. Springer. Pages 124-144.
4. Gibbons, Jeremy. Wu, Nicholas. Folding domain-specific languages: Deep and shallow embedding (functional pearl). (2014).
5. Menrik, Marjan. Heering, Jan. Sloane, Anthony. When and How to Develop Domain-Specific Languages. (2005) ACM Computing Surveys, vol.37. Pages 316-344.
6. Lane, Saunders M. Categories for the Working Mathematician. Second Edition. Springer. {insert pages here}

