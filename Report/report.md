# Introduction of Recursion schemes in Domain Specific Language.

## Introduction
The most successful computer systems can stand the test of time;
it requires numerous people to work together to test, maintain and update it throughout its lifespan.
And abstraction,
which is one of the most ubiquitous themes in computer science,
provides a good strategy to do this by managing its complexity.
Thus it comes as no surprise that the most successful modern programming languages depend on and exploit this
by introducing a variety of abstraction techniques,
each of which has encompassed the idea of generality.  

Although generality is a good thing,
a generic approach that provides solutions in multiple problem domains could mean that the solution is not an optimal one,
that their very generality could count against them.
This motivates the idea of programming languages designed specifically for a certain application,
that is,
it should capture the entirety of the semantics of the problem domain:
it is _domain specific_.

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

It is extremely common to have nested data structures in a problem domain,
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

The purpose of this essay is to introduce the idea of recursion schemes.
The structure of the report is as follows:

1. Abstract concepts from Category theory which has found its way into Haskell's list of abstraction techniques (that are used in this report) will be introduced in the first section.
2. "Regular" data types can be rewritten using ideas from lambda calculus.
3. Meijer et al introduced a set of recursion schemes: catamorphism, anamorphism, paramorphism, apomorphism. We will show its implementation in Haskell.
4. Conclusion, we will reflect on recursion schemes and its uses in a domain specific language.

## Crash course category theory (Draft)
Category Theory is a branch of mathematics infamous for its abstract nonsense,
the idea came from abstracting concepts from algebraic topology.
Regardless of its origins, it has proved very useful in computer science.
Since, as programmers, we work with functions throughout our daily life and each function has the idea of composition (as long as their types match)
This is exactly the structure the category theory captures.
Unsurprisingly, ideas from this theory can be exploited,
an example is the very recursion schemes that we will be studying.

A category is an algebraic structure defined on a collection of objects and morphisms (or arrows)
that can be thought of as special functions between objects.
There are also other properties the morphisms must satisfy such as associativity and composition.
Because of the abstract nature of a category,
it can be used as a model of computation,
where the types can be modeled as objects in a category and program expressions, our morphisms.

The concept of a functor is to serve as a structure preserving map between categories.
It must therefore preserve the relationship between the objects,
arrows and the additional properties of each morphism.
This structure preserving nature of the functor is captured by the functor laws,
that each and every functor must hold true.
A functor, specifically an endofunctor,
is used represent type constructors in our high-level programming language.
Because of the importance of functors in category theory,
it is unsurprising that they are ubiquitous in programming,
certain functors can be further generalised into applicative functors,
and even monads,
which provide considerable more power but is sparse in comparison.

In Haskell,
a functor is defined as follows:

```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

It is also important when providing an instance of functor to ensure that they satisfy the functor laws of:

```
   fmap id = id
   fmap (f . g) = fmap f . fmap g
```

The first law respects the idea of an identity in morphism
and the second preserves the idea of composition of morphism.

{introduction for algebra and coalgebra} 
In mathematics, abstract structures such as groups are given an algebraic specification, this constitutes finite data.
Specifications can be given to programmers type
Each specification is an example of a categorical algebra,
Many data types have models which arise as _initial algebras_, (REPHRASE THIS)
a particular subset of algebras that has a very interesting property:
there is a unique homomorphism from 
this property gives us the framework for induction and recursion.

The categoric dual of the initial algebra is the _final co-algebra_.


## Hiding (explicit) recursion
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
this is called the _pattern functor_ which is almost identical to the original `Expr`.
However, `ExprF` is not quite equivalent, we need to somehow arbitrarily nest `ExprF` in the definition.
We will use the Y combinator to do this.

In lambda calculus, it is not possible to refer to the function definition in its body; there is no feature for (explicit) recursion.
However,
by using Y combinator,
we can replicate a recursive function.  
It is, by definition, a higher-order function that takes a non-recursive function that satisfies the following:

$$Y f = f (y  f) \forall f$$

This is an idea can be transferred into Haskell's type system to help “fix” our pattern functor by describing the Y combinator,
we can now describe `Fix ExprF` which is isomorphic to `ExprF` by using:

```
Fix f = In (f (Fix f))
```

This technique of redefining recursive data types is very powerful,
especially for recursion schemes, where we can capture the pattern in a single combinator.

## Recursion Schemes

### Catamorphism
Meijer et al introduced the idea of the generalised fold called a catamorphism (from the greek preposition for destruction).
In Category Theory, it denotes the unique homomorphism from an initial algebra into some other algebra.

### Anamorphism
The categoric dual of the catamorphism. Generalises unfold.

### Paramorphism
Generalises recursion

### Apomorphism
The categoric dual of the apomorphism.

\pagebreak

## Reference
1. Shaw, Mary. Abstraction Techniques in modern programming languages. (1984). IEEE Software. Pages 10-26.
2. Van Deursen, Arie. Klint, Paul. Visser, Joost. Domain-Specific Languages: An Annotated Bibliography. (2000). ACM SIGPLAN Notices.
3. [1] Meijer, Erik. Fokkinga, Maarten. Paterson, Ross. Functional programming with bananas, lenses, envelopes and barbed wire. (1991). FPLCA. LNCS, vol. 523. Springer. Pages 124-144.
4. Gibbons, Jeremy. Wu, Nicholas. Folding domain-specific languages: Deep and shallow embedding (functional pearl). (2014).
5. Menrik, Marjan. Heering, Jan. Sloane, Anthony. When and How to Develop Domain-Specific Languages. (2005) ACM Computing Surveys, vol.37. Pages 316-344.
6. Lane, Saunders M. Categories for the Working Mathematician. Second Edition. Springer. {insert pages here}

