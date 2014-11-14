Bayesian scoring?
=================

### Definitions

Let Tree(C,L) be the type of trees with node labels C, node arity
arity : C -> N, and leaf type L.  That is, A = Tree(C,L) means

  A = Leaf(L) | sum(c in C) c(A^arity(l))

Given a tree type A = Tree(C,L), Skel(A,n) is the type of shallow skeletons

  Skel(A,n) = Tree(C,n)

That is, an s in Skel(A,n) is a tree with the same constructors as
A but with numbered "holes" in place of leaves taken from 0 to n-1.
Given a skeleton s, we can apply it to n arguments x(0) to x(n-1) as

  s(xk) = map (k -> xk)

Dist(T) is the type of (finite) probability distributions over T.

Finally, we define a max-argmax function

  maxa(f) = max(f),argmax(f)

### The user

The user acts as a random function

  f : A -> Dist(B)

where A and B are two types of trees

  A = Tree(AC,AL)
  B = Tree(BC,BL)

A is the type that the user "had in mind" and B is the type that we
see as input, distorted appropriately by f.  We assume f is given
recursively by

  fl : AL -> Dist(BL)
  fc : AC -> Dist(Skel(B,N))

  f(Leaf(x)) = Leaf(fl(x))
  f(c(xk)) = fc(independent f(xk))

That is, we have a random function on leaves, and shallow distributions
for each constructor c in AC.  We assume that fc contains no leaves.


### Frequentist inference

We seek

  g : B -> (R,A)
  g(b) = maxa(a in A) Pr(f(a) = b)

Consider b in B.  If b = Leaf(x), we know that b = Leaf(fl(a)), so

  g(Leaf(x)) = Leaf(gl(x))
  gl : BL -> (R,AL)

Given b and a skeleton s, subs is a function returning the subtrees of b 
corresponding to the holes in s:

  subs: B x Skel(B,n) -> B^n

Otherwise, we first ask which constructors c in AC could have generated
b based on the top level structure of b.  For each such c, we have

  ps(c,i),sk(c,i) = fc(i)               # distribution of skeletons (partial b) generating c
  b(c,i,k) = subs(b,sk(c,i))            # find subtrees of b matching the skeleton holes
  p(c,i,k),a(c,i,k) = g(b(c,i,k))       # get best interpretation of lower levels
  a(c,i) = c(a(c,i,k))                  #
  p(c,i) = ps(c,i) * prod(k) p(c,i,k)   # ...p(c,i) is increasing in p(c,i,k)

which defines

  p,a = g(b) = maxa (a(c,i) -> p(c,i))


### Bayesian inference

We seek

  g : B -> Dist(A)
  g(b,a) = Pr(a | b) = Pr(b | a) Pr(a) / sum(a') Pr(b | a') Pr(a')

Leaves go via a subfunction

  gl : BL -> Dist(AL)

Constructor cases (c,i) are similar to frequentist:

  p(c,i),sk(c,i) = fc(i)
  sk(c,i)(b(c,i,k)) = b   # Skeleton pattern match
  a(c,i) = c(a(c,i,k))

  Pr(b | a) = Pr(b | c,a(c,i,k)) = sum(i) p(fc(i)) * prod(k) Pr(b(c,i,k) | a(c,i,k))

  Pr(a | b) = Pr(b | a) Pr(a) / sum(a') Pr(b | a') Pr(a')

The trick here is that the above formulation doesn't yet reflect the need for
"brevity" in the computed distribution Dist(A).  Let's hold off on thinking about
the Bayesian case for a while.


### Frequentist filtering

The above frequentist discussion is purely top down and ignores the issue of higher
level constructors being ruled out by lower level choices (filtering).  I believe
this is straightforward to add, but will hold off working it out until the need
arises.
