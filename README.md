[API reference](https://polytypic.github.io/loko-ml/doc/loko/Loko/index.html)

# Lower-Kinded Optics for OCaml

An optic is an abstraction that knows how to take apart or transform a data
structure in some particular way and then put the data structure back together
to provide _focuses_ for
[CRUD](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete)
operations. Optics for complex data structures can be built compositionally
following the structure and respecting the invariants of the data structure.

This library uses an approach to implementing optics that provides the following
features:

- Optics are functions and can be composed with ordinary function composition
- Optics have simple polymorphic types
- Optic classes:
  - **Isomorphisms** bidirectionally targeting the whole transformed data
  - **Lenses** targeting a single focus
  - **Prisms** optionally targeting a matching focus
  - **Traversals** targeting arbitrary numbers of focuses
- Operations over optics:
  - (**C**) Setting an empty focus to a new value
  - (**R**) Viewing a focus or folding over focuses
  - (**U**) Mapping over focuses to update them
  - (**D**) Requesting to remove focuses

## Introduction

The following subsections introduce some aspects of Loko via simple examples.

### Basics

To begin, we bind the `Loko` library module to the module abbreviation `L`:

```ocaml
module L = Loko
```

This is not strictly necessary, of course, but it helps to keep things a bit
more concise. The Loko library does not provide a large number of infix
operators, so one can usually get away without opening the `Loko` module.

As a first example, let's just try out the `L.fst` and `L.snd` *optic*s. They
are a pair of optics that focus on the first and second elements of a pair,
respectively. Using the `L.view` _operation_ we can then use those optics to get
the first

```ocaml
# L.view L.fst (42, "answer")
- : int = 42
```

and second

```ocaml
# L.view L.snd (42, "answer")
- : string = "answer"
```

element of a pair of _data_.

On the other hand, using the `L.over` operation we can use those optics to map
over or modify the first and second element of a pair and obtain a new pair. For
example:

```ocaml
# L.over L.fst Int.to_string (42, "answer")
- : string * string = ("42", "answer")
```

To summarize, we use an _optic_ to specify focuses inside a _data_ structure for
an _operation_ to operate on:

```
operation optic data
    ▲       ▲    ▲
    ┃       ┃    ┃
    ┃       ┃    ┗━━ The whole data structure we operate on
    ┃       ┃
    ┃       ┗━━ What parts of the data structure we want to focus on
    ┃
    ┗━━ What operation we want to perform on the focuses
```

### Types of optics

In the previous section we used the `L.over` operation with the `L.fst` optic to
update the first element of a pair such that the type of the element became
`string` instead of the `int` as in the original pair. Optics in Loko generally
allow one to perform *polymorphic update*s where possible. Let's take a closer
look at the types of optics.

The signatures of `L.fst` and `L.snd` can be written as

```ml
val fst : ('L1 * 'R, 'L1, 'L2, 'L2 * 'R) optic
val snd : ('L * 'R1, 'R1, 'R2, 'L * 'R2) optic
```

where `optic` is the type abbreviation

```ml
type (-'S, +'F, -'G, +'T) optic = ('F, 'G) pipe -> ('S, 'T) pipe
```

and `pipe`

```ml
type (-'S, +'T) pipe
```

is an opaque type.

With four type parameters the `optic` type likely seems puzzling. Let's break it
down. The following shows the meanings of the type parameters:

```
(-'S, +'F, -'G, +'T) optic
  ▲    ▲    ▲    ▲
  ┃    ┃    ┃    ┃
  ┃    ┃    ┃    ┗━━ Type of output data structure in a write operation
  ┃    ┃    ┃
  ┃    ┃    ┗━━ Type of values to replace focuses in a write operation
  ┃    ┃
  ┃    ┗━━ Type of values in focuses to operate on
  ┃
  ┗━━ Type of data structure given as input
```

So, looking at e.g. the signature of `L.fst`

```ml
val fst : ('L1 * 'R, 'L1, 'L2, 'L2 * 'R) optic
```

we can see that it takes a pair `'L1 * 'R` as input and selects focuses of type
`'L1` from that input. In case of a write operation, those focuses can take
values of type `'L2` and the end result of a write operation is a pair of type
`'L2 * 'R`.

Note that the type does not say how many focuses `L.fst` has. In the case of
`L.fst` it is exactly one, which makes `L.fst` a _lens_, but this is not part of
the type.

In summary, optics are functions and the `optic` type abbreviation takes four
type parameters that specify the type of the whole input, the type of focuses
selected from input, the type of values potentially written to focuses, and the
type of the resulting output, respectively.

### Nested composition of optics

What makes the separation of optics from operations interesting is the ability
to compose optics. As we just saw in the previous section optics in Loko are
"just" functions. In this section we will see that we can also compose optics
just like ordinary functions.

The `Infix` submodule of Loko provides the `%` operator for function
composition. Let's open it for convenience:

```ocaml
open L.Infix
```

The `%` operator symbol was chosen as it seems to be provided by some existing
OCaml libraries.

What function composition allows us to do with optics is to compose them in
order to operate on nested data structures. For example, let's say that we want
to focus on the second element of the first pair of nested pairs. We can compose
an optic for that as `L.fst % L.snd` and use that with various operations. For
example, we can use `L.view`

```ocaml
# L.view (L.fst % L.snd) ((3, "1"), ('4', true))
- : string = "1"
```

to view such a nested focus or we could use `L.over`

```ocaml
# L.over (L.fst % L.snd) int_of_string ((3, "1"), ('4', true))
- : (int * int) * (char * bool) = ((3, 1), ('4', true))
```

to modify such a nested focus.

Using ordinary function composition to deal with nested data structures isn't
the only way to compose or build more complex optics from simpler optics, but it
is probably the most common.

To summarize, using function composition we can compose optics in order to
operate on nested data structures. Furthermore, to emphasize a previously made
point, a single optic composition may be used with many different operations.
This separation of the selection of focuses from the operation to perform on
them is what can make using optics preferable over traditional methods when
operating on complex data structures.

### Traversals and folds

We have so far only used optics called lenses that always have a single focus.
Traversals are optics that have arbitrarily many focuses. For example, the
`L.List.elems` traversal focuses on all the elements of a list. Using the
`L.over` operation with the `L.List.elems` traversal we can map over a list:

```ocaml version>=5.0.0
# L.over L.List.elems (( + ) 1) [2; 0; 3]
- : int list = [3; 1; 4]
```

Traversals can also be composed with other optics. For example, given a list of
pairs, we could use the composition `L.List.elems % L.fst` to operate on all the
first elements of pairs on a list:

```ocaml version>=5.0.0
# L.over (L.List.elems % L.fst) (( + ) 1) [(3, 2); (6, 6)]
- : (int * int) list = [(4, 2); (7, 6)]
```

The potential number of focuses an optic composition has works like
multiplication and composing a traversal with any other optic also yields a
traversal.

Aside from using `L.over` to map over all the focuses of a traversal we can also
use `L.view` to extract the first, if any, focus:

```ocaml
# L.view L.List.elems [4; 1]
- : int = 4
```

In case there are no focuses, `L.view` fails with an exception. If such a case
is to be expected, one should e.g. use `L.view_opt`, which returns `None` in
case there are no focuses

```ocaml
# L.view_opt L.List.elems []
- : 'a option = None
```

or `Some`

```ocaml
# L.view_opt L.List.elems [1; 1; 2]
- : int option = Some 1
```

in case there is at least one.

More generally we can also _fold_ over the focuses of a traversal. For example,
we could use `L.fold` with `L.List.elems % L.fst` to sum the first elements of
pairs of a list:

```ocaml
# L.fold 0 ( + ) (L.List.elems % L.fst) [(3, 2); (6, 6)]
- : int = 9
```

Or we could use `L.collect` to get a list of the focuses:

```ocaml
# L.collect (L.List.elems % L.fst) [(3, 2); (6, 6)]
- : int list = [3; 6]
```

Notice again how the optic composition, `L.List.elems % L.fst`, remains the same
while the operation performed varies.

It is also possible to use `L.parts_of` to convert a traversal into a lens that
produces an array of the focuses:

```ocaml
# L.view (L.parts_of (L.List.elems % L.fst)) [(3, 2); (6, 6)]
- : int array = [|3; 6|]
```

Doing so allows the array to be treated as a whole. We could, for example, view
it as a list using `L.Array.as_list` and then use `L.over` with `List.rev` to
reverse the order of values in the focuses:

```ocaml version>=5.0.0
# L.over (L.parts_of (L.List.elems % L.fst) % L.Array.as_list) List.rev
    [(3, 2); (6, 6)]
- : (int * int) list = [(6, 2); (3, 6)]
```

In summary, lenses, which always have a single focus, are not the only class of
optics. Traversals are optics that have arbitrary numbers of focuses. Different
classes of optics can be composed together. We can perform a fold over the
focuses an optic selects from a data structure.

### Isomorphisms

Isomorphisms, like lenses, always have a single focus. The difference is that
with an isomorphism the focus is the whole data structure given as input to the
isomorphism. Conversely, the output of an isomorphism is not dependent on the
input and is determined solely by the value written through the isomorphism.
This means that the read and write directions of an isomorphism are both just
single parameter functions and it is possible to implement them in such a way
that they can be inverted &mdash; reversing the read and write directions.

For example, given an isomorphism `plus 2`, where `plus` is implemented as

```ocaml
let plus n = L.iso (fun x -> x + n) (fun x -> x - n)
```

we can not only read and write through it

```ocaml
# L.view (plus 2) 1
- : int = 3
# L.set (plus 2) 3 42
- : int = 1
```

but we can also use `L.re` to invert it and still read and write through it:

```ocaml
# L.view (L.re (plus 2)) 3
- : int = 1
# L.set (L.re (plus 2)) 1 42
- : int = 3
```

`L.re` only works on isomorphisms, created with `L.iso`, or prisms, which we'll
talk about in the next section, and with compositions of such optics. An attempt
to invert other kinds of optics will raise.

Instead of using `L.set` to run an isomorphism in the inverted direction we can
also use the `L.review` operation which doesn't require the (ignored) initial
input:

```ocaml
# L.review (plus 2) 3
- : int = 1
```

Isomorphisms in Loko do not need to be strictly invertible functions. For
example, the `L.truncate` isomorphism truncates `float`s to `int`s or, in the
other direction, converts `int`s to `float`s. Both of these directions may be
lossy.

```ocaml
# (1 lsl 61) = ((1 lsl 61) + 1)
- : bool = false
# L.review L.truncate (1 lsl 61) = L.review L.truncate ((1 lsl 61) + 1)
- : bool = true
```

Isomorphisms can also be composed with other optics and the end result generally
has the same properties as the other optics.

In summary, isomorphisms target the whole data structure given as input and can
be inverted. Isomorphisms can be composed with other optics.

### Prisms

The kinds of optics we've discussed so far work in a kind of unconditional
manner. In particular, lenses and isomorphisms always have a single focus. Also,
basic traversals over collections focus on all the elements of a collection.
What if there isn't always a value to focus on, such as can be the case with sum
types, like `option`al values? Or how would one go about conditional selection
of focuses from a collection? Focusing on an optional value is not going to work
with a lens or an isomorphism. A traversal can focus on an optional value, but
there is a more specific class of optics called *prism*s that are not only
traversals, but also have at most one focus and can be inverted like
isomorphisms.

Indeed, the traversal over the elements of an option is also a prism, and can be
inverted:

```ocaml
# L.review L.Option.elems 1
- : int option = Option.Some 1
```

TODO

### Removal of focuses

A special feature of the Loko library is that it supports removal of focuses. If
you are coming from some other optics library, that may sound puzzling. Let's
build up our understanding via a series of examples. Suppose we have an array
that contains lists of integers. We can write a simple traversal that targets
those and use a fold to collect them:

```ocaml
# L.collect
    (L.Array.elems
    % L.List.elems)
    [| [3; -1; 1]; [-2; -3]; [-4; 4] |]
- : int list = [3; -1; 1; -2; -3; -4; 4]
```

Suppose then that we want to remove the negative elements. We continue by
refining the traversal to accept only the negative elements:

```ocaml
# L.collect
    (L.Array.elems
    % L.List.elems
    % L.accept (fun x -> x < 0))
    [| [3; -1; 1]; [-2; -3]; [-4; 4] |]
- : int list = [-1; -2; -3; -4]
```

To remove the negative elements we just use the `L.remove` operation:

```ocaml version>=5.0.0
# L.remove
    (L.Array.elems
    % L.List.elems
    % L.accept (fun x -> x < 0))
    [| [3; -1; 1]; [-2; -3]; [-4; 4] |]
- : int list array = [|[3; 1]; []; [4]|]
```

Suppose further that we'd also like to remove any empty lists from the array. We
can achieve that e.g. by composing `L.as_removed []` before the list traversal:

```ocaml version>=5.0.0
# L.remove
    (L.Array.elems
    % L.as_removed []
    % L.List.elems
    % L.accept (fun x -> x < 0))
    [| [3; -1; 1]; [-2; -3]; [-4; 4] |]
- : int list array = [|[3; 1]; [4]|]
```

`L.remove o s` is actually a shorthand for `L.over (o % L.removed) ignore s`. In
other words, an optic can, by itself, signal removal during a write operation.
`L.as_removed value` signals removal when the element written through it is
equal to the given `value`.

The way removal works is that during a write operation a focus can be "signaled"
as being removed. That "signal" needs to be handled. Not all optics can handle
removal. For example, what if you signal the removal of the first element of a
pair? Well, it cannot work, of course, as we can verify using `L.can_remove`:

```ocaml
# L.can_remove L.fst ("Computer", "says no")
- : bool = false
```

Optics that cannot handle removal propagate the signal upwards and if no optic
handles it, no result can be produced for the operation. Generally speaking
traversals for collection types, such as lists, arrays, and options, can handle
removal. For example, if we put a pair inside an option, removal can be done:

```ocaml
# L.can_remove (L.Option.elems % L.fst) (Some ("Computer", "says no"))
- : bool = true
```

The result is the removal of the whole pair:

```ocaml
# L.remove (L.Option.elems % L.fst) (Some ("Computer", "says no"))
- : ('a * string) option = Option.None
```

We could also have the option inside the first element of the pair:

```ocaml
# L.remove (L.fst % L.Option.elems) (Some ("Computer"), "says no")
- : 'a option * string = (Option.None, "says no")
```

It is also possible to compose an optic to explicitly handle removal in some
way. For example, `L.removed_as_none` changes the type of the focus on write:

```ocaml
# L.remove (L.fst % L.removed_as_none) ("Computer", "says no")
- : 'a option * string = (None, "says no")
```

Another option is to replace a removed focus with a given value using
`L.removed_as`:

```ocaml
# L.remove (L.fst % L.removed_as "Nobody") ("Computer", "says no")
- : string * string = ("Nobody", "says no")
```

In summary, Loko has special support to request removal of focuses. Certain
optics signal removal. The signal to remove an element needs to be handled by
another optic in the composition. Some optics handle removal by default and
there are optics specifically for specifying how to handle removal.

### Maintaining invariants

```ocaml
let eta'1 fn x1 x2 = fn x1 x2
```

```ocaml
type ('k, 'v) bt = [ `Lf | `Br of ('k, 'v) bt * 'k * 'v * ('k, 'v) bt ]
```

```ocaml
let a_tree : (_, _) bt =
  `Br
    ( `Br (`Lf, 2, "s", `Br (`Lf, 4, "o", `Lf)),
      5,
      "g",
      `Br (`Lf, 7, "i", `Br (`Lf, 11, "c", `Lf)) )
```

```ocaml
let on_br p =
  p |> L.prism (function `Br x -> Right x | `Lf -> Left `Lf) (fun x -> `Br x)
```

```ocaml
let key p = p |> on_br % L.elem_2_of_4
let smaller p = p |> on_br % L.elem_1_of_4
let greater p = p |> on_br % L.elem_4_of_4
```

```ocaml
let rec naive_bst p =
  L.rewrite (function
    | `Lf -> `Lf
    | `Br (l, k, Some v, r) -> `Br (l, k, v, r)
    | `Br (`Lf, _, None, t) | `Br (t, _, None, `Lf) -> t
    | `Br (`Br (l, k, v, m), _, None, r) ->
      L.set (node_of k) (`Br (l, k, Some v, m)) r)
  @@ L.removed_as `Lf
  @@ p

and node_of k' =
  L.cond_of key
  @@ L.case (fun k -> k' < k) (smaller % eta'1 node_of k')
  @@ L.case (fun k -> k < k') (greater % eta'1 node_of k')
  @@ L.otherwise naive_bst
```

```ocaml
let value_of k =
  node_of k
  % L.lens
      (function `Lf -> None | `Br (_, _, v, _) -> Some v)
      (fun v -> function
        | (`Lf as l as r) | `Br (l, _, _, r) -> `Br (l, k, v, r))
  % L.removed_as_none
```

```ocaml
let rec inorder p =
  p
  |> naive_bst % on_br % L.branch'4 inorder L.zero L.removed_as_none inorder
```

```ocaml
# a_tree
  (* Update: *)
  |> L.set (value_of 2) "M"
  (* Delete: *)
  |> L.remove (value_of 4)
  (* Create: *)
  |> L.set (value_of 3) "a"
  (* Read: *)
  |> L.concat "-" inorder
- : string = "M-a-g-i-c"
```

## On naming

- `to_x` + `of_x` -> `as_x`

## Background

The basic implementation technique used in this library was originally developed
in a C# project (not available as open-source). Later a
[prototype F# library](https://github.com/polytypic/NetOptics) was also
developed. This version attempts to take the approach further.

The implementation technique has two nice properties: optics are just functions
and higher-kinded types are not needed. On the other hand, compared to some
other approaches to implementing optics, the class (or the number of focuses) of
an optic is not encoded in the type (although it could likely be done using
phantom types). This means that operations that e.g. require an optic to have at
least one focus on given data or that require an optic to be invertible may
raise exceptions.

The goal to support removing focuses comes after realizing the usefulness of the
ability from working with
[partial.lenses](https://github.com/calmm-js/partial.lenses). In partial.lenses
the ability comes naturally due to the structural nature of data in JavaScript
and being able to
[treat all optics as partial](https://github.com/calmm-js/partial.lenses#on-partiality)
(or optional). In this library the technique is more of an add-on feature
requiring more glue to make sure nominal types check out.
