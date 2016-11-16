# Introduction of Recursion schemes in Domain Specific Language.

## Introduction
The most successful computer systems can stand the test of time;
it requires numerous people to work together to test, maintain and update it throughout its lifespan.
Abstraction,
one of the most ubiquitous concept in computer science,
provides a good strategy to ensure its survival by managing its complexity.
Thus it comes as no surprise that the most successful programming languages exploit this
by introducing a variety of abstraction techniques for the programmer,
each of which encompass the idea of generality.  

Although generality is a good thing,
a generic approach in multiple problem domains could mean that the solution is not an optimal one,
that their very generality could count against them.
This motivates the idea of programming languages designed specifically for a certain application,
that is,
it should capture the entirety of the semantics of the problem domain:
it is _domain specific_.

Domain specific languages (DSL) are often small languages that over a specific domain offer more expressivity (than general purpose language). 

There are two main ways of implementing a DSL: 

* Standalone: This is the classical approach; it involves implementing the domain specific language from scratch.

  {expand here}

* Embedded languages: An existing general purpose programming language with all its constructs is used as a “based” to build a domain specific language.

  An embedding programming language offer their expressive domain specific power as well as the additional power of its base general purpose language.

There are two main approaches to implementing an embedded DSL:

* Shallow: Its semantics are implemented directly so they are represented as functions. 
* Deep: It is implemented by constructing an abstract syntax tree that represents the expression tree.

In shallow embedding the semantics depend directly on the semantics of the composed term:
it only captures compositional semantics.
In contrast,
it is possible to define a complex interpretation,
that is non-compositional,
of deep embedding that cannot be represented in shallow embedding.
Even though this is possible, Gibbons et al say that they are equivalent.

It is extremely common to have nested data structures in a problem domain,
so it is common to define an equivalent data representation in the DSL.
However,
to traverse such a structure,
we require a recursive function.
Because of their abundance,
programmers often do not notice that they are using recursive functions so the idea of generalising traversals could replace a large amount of type specific functions that utilises recursion.

Meijer et al uses simple yet powerful abstract nonsense in category theory to introduced the idea of recursion schemes,
a set of combinators that formulates the process of traversing and evaluating sophisticated inductive data structures.[3][1].
This generalisation allows us to structure our program in a well-defined way by decoupling how a function recurse over a data structure from the underlying purpose of the function meaning,
this means programmers can just focus on what the function is actually does.

The authors begin to criticising those who functionally program without recursion schemes,
comparing them to imperative programmers who use gotos.
In Djikstra’s letter to the ACM where he expressed his concerns that goto masks the flow of the program;
here the use of unstructured (explicit) recursion obscures the underlying algorithmic structure,
making it hard to prove arbitrary properties of the function,
the analogy here is appropriate.

The purpose of this essay is to introduce the idea of recursion schemes.
The structure of the report is as follows:

1. Abstract concepts from Category theory has found its way into Haskell's list of abstraction techniques (that are used in this report) will be introduced in the first section.
2. "Regular" data types can be rewritten using fix points.
3. Meijer et al introduced a set of recursion schemes: catamorphism, anamorphism, paramorphism, apomorphism. We will show its implementation in Haskell.
4. Conclusion, we will reflect on recursion schemes and its uses in a domain specific language.

## Crash Course: Category Theory (Draft)
Abstracting concepts from algebraic topology has yielded nothing but "abstract nonsense",
these ideas  has been formulised to form the infamous cathegory theory 
which has proved very useful in computer scientists.
Since, as programmers, we work with functions throughout our daily life and each function (as long as their types match) can be composed.
This is exactly the structure that category theory captures.
Unsurprisingly, these ideas can be exploited.

But what is a category? It is an algebraic structure defined on a collection of objects and morphisms
that can be thought of as special functions between objects that perserves its structure,
these morphisms must also satisfy associativity and composition.
Because of this, many category theorists believe that the morphisms are of greater significance than objects 
because they reveal their true structure.

It is natural to consider a structure perserving map between categories,
leading us to the idea of functors.
It must therefore additionally preserve the relationship between the objects,
morpisms and its properties:
this is captured by the functor laws,
that each and every functor must satisfy.

{Expand this out plus other}
Because of the abstract nature of a category,
it can be used as our model of computation,
where the types corresponds to objects in a category and program expressions, our morphisms.

A functor, specifically an endofunctor,
is used to model type constructors in our high-level programming language.
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
### Pattern Functors
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

### Fix points of functors

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

## References
1. Shaw, Mary. Abstraction Techniques in modern programming languages. (1984). IEEE Software. Pages 10-26.
2. Van Deursen, Arie. Klint, Paul. Visser, Joost. Domain-Specific Languages: An Annotated Bibliography. (2000). ACM SIGPLAN Notices.
3.  Meijer, Erik. Fokkinga, Maarten. Paterson, Ross. Functional programming with bananas, lenses, envelopes and barbed wire. (1991). FPLCA. LNCS, vol. 523. Springer. Pages 124-144.
4. Gibbons, Jeremy. Wu, Nicholas. Folding domain-specific languages: Deep and shallow embedding (functional pearl). (2014).
5. Menrik, Marjan. Heering, Jan. Sloane, Anthony. When and How to Develop Domain-Specific Languages. (2005) ACM Computing Surveys, vol.37. Pages 316-344.
6. Lane, Saunders M. Categories for the Working Mathematician. Second Edition. Springer. {insert pages here}

