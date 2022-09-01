(** {1 Loko — Lower-Kinded Optics}

    Typically one either refers to the library primitives through the [Loko]
    module name or, as done in this documentation, uses an abbreviation such as
{[
    module L = Loko
]}
    to keep code more succinct.

    To compose optics we also define an ordinary function composition operator
{[
    let ( ^<< ) f g x = f (g x)
]}
    as one is not exposed by [Loko]. *)

(** {2 Auxiliary abstractions}

    {b NOTE:} The abstractions in this section serve auxiliary roles for the
    definition of optics. *)

(** An iterator extracts elements of type ['x] one-by-one from a collection of
    type ['xs]. *)
type ('xs, 'x) iterator =
  | Iterator : {
      start : 'xs -> 'iterator;
      next : 'iterator -> [`Done | `Next of 'x * 'iterator];
    }
      -> ('xs, 'x) iterator

module Iterator : sig
  val array : ('a array, 'a) iterator
  (** Array iterator. *)

  val list : ('a list, 'a) iterator
  (** List iterator. *)
end

(** A builder accumulates elements of type ['y] one-by-one to produce a
    collection of type ['ys]. *)
type ('y, 'ys) builder =
  | Builder : {
      empty : unit -> 'builder;
      add : 'y -> 'builder -> 'builder;
      finish : 'builder -> 'ys;
    }
      -> ('y, 'ys) builder

module Builder : sig
  val array : ('a, 'a array) builder
  (** Array builder. *)

  val list : ('a, 'a list) builder
  (** List builder. *)
end

(** {2 Optic types}

    *)

type (-'S, +'T) pipe
(** The [pipe] type constructor hides most of the implementation details of
    optics and basically represents a function that takes values of type ['S] as
    input and produces values of type ['T] as output. *)

type (-'S, +'F, -'G, +'T) optic = ('F, 'G) pipe -> ('S, 'T) pipe
(** Optics are functions over {!pipe}s and are composable with normal function
    composition. *)

(** {2 Optic constructors}

    *)

val iso : ('S -> 'F) -> ('G -> 'T) -> ('S, 'F, 'G, 'T) optic
(** Defines a new invertible isomorphism from a pair of conversion functions.
{[
    let plus n = L.iso ((+) n) (Fun.flip (-) n)
    let 3 = L.view (plus 1) 2
    let 1 = L.review (plus 2) 3
    let 1 = L.view (L.re (plus 2)) 3
]} *)

val lens : ('S -> 'F) -> ('G -> 'S -> 'T) -> ('S, 'F, 'G, 'T) optic
(** Defines a new lens from a getter and setter. *)

val getter : ('S -> 'F) -> ('S, 'F, 'G, 'S) optic
(** Defines a new getter. *)

val fold_lens :
  (('S, 'E, 'G, 'T) optic -> 'S -> 'F) ->
  ('S, 'E, 'G, 'T) optic ->
  ('S, 'F, 'G, 'T) optic
(** [fold_lens fold traversal] defines a lens out of a suitable [fold] and a
    [traversal]. *)

val prism :
  ('S -> [< `Hit of 'F | `Miss of 'T]) -> ('G -> 'T) -> ('S, 'F, 'G, 'T) optic
(** Defines a new invertible prism from a constructor-destructor pair.
{[
    let ends_with suffix =
      L.prism
        (fun string ->
          if Filename.check_suffix string suffix then
            `Hit (Filename.chop_suffix string suffix)
          else `Miss string)
        (fun prefix -> prefix ^ suffix)

    let "Loko" = L.view (ends_with ".ml") "Loko.ml"
    let "Loco.ml" = L.set (ends_with ".ml") "Loco" "Loko.ml"

    let false = L.can_view (ends_with ".ml") "Loko.mli"
    let "Loko.mli" = L.set (ends_with ".ml") "Loco" "Loko.mli"
]} *)

val traversal : ('S, 'F) iterator -> ('G, 'T) builder -> ('S, 'F, 'G, 'T) optic
(** Defines a new traversal from an iterator-builder pair. *)

val branch'2 :
  ('F1, 'F, 'G, 'G1) optic ->
  ('F2, 'F, 'G, 'G2) optic ->
  ('F1 * 'F2, 'F, 'G, 'G1 * 'G2) optic
(** Defines a new traversal over the elements of a 2-tuple. *)

val branch'3 :
  ('F1, 'F, 'G, 'G1) optic ->
  ('F2, 'F, 'G, 'G2) optic ->
  ('F3, 'F, 'G, 'G3) optic ->
  ('F1 * 'F2 * 'F3, 'F, 'G, 'G1 * 'G2 * 'G3) optic
(** Defines a new traversal over the elements of a 3-tuple. *)

val branch'4 :
  ('F1, 'F, 'G, 'G1) optic ->
  ('F2, 'F, 'G, 'G2) optic ->
  ('F3, 'F, 'G, 'G3) optic ->
  ('F4, 'F, 'G, 'G4) optic ->
  ('F1 * 'F2 * 'F3 * 'F4, 'F, 'G, 'G1 * 'G2 * 'G3 * 'G4) optic
(** Defines a new traversal over the elements of a 4-tuple. *)

(** {2 Viewing a focus}

    *)

val can_view : ('S, 'F, 'G, 'T) optic -> 'S -> bool
(** Determines whether the optic has a focus on the data. *)

val view : ('S, 'F, 'G, 'T) optic -> 'S -> 'F
(** Extracts the first focus of the optic on the data. Raises when there is no
    focus. *)

val view_opt : ('S, 'F, 'G, 'T) optic -> 'S -> 'F option
(** Attempts to extract the first focus of the optic on the data. *)

(** {2 Reviewing through invertible optics}

    *)

val can_review : ('S, 'F, 'G, 'T) optic -> 'G -> bool
(** Determines whether the optic has an inverse focus on the data. *)

val review : ('S, 'F, 'G, 'T) optic -> 'G -> 'T
(** Views through isomorphism in inverse direction. Raises when the optic is not
    reversible or doesn't have inverse focus on the data. *)

val review_opt : ('S, 'F, 'G, 'T) optic -> 'G -> 'T option
(** Attempts to extract the inverse focus of the optic on the data.  WARNING:
    This will raise when the optic is not reversible. *)

(** {2 Mapping over focuses}

    *)

val can_over : ('S, 'F, 'G, 'T) optic -> ('F -> 'G) -> 'S -> bool
(** Determines whether the focuses of the optic on the data can be updated. *)

val over : ('S, 'F, 'G, 'T) optic -> ('F -> 'G) -> 'S -> 'T
(** Updates the focuses of the optic on the data. Raises when cannot. *)

val over_default : ('S, 'F, 'G, 'S) optic -> ('F -> 'G) -> 'S -> 'S
(** Updates the focuses of the optic on the data. Returns input when cannot. *)

val over_opt : ('S, 'F, 'G, 'T) optic -> ('F -> 'G) -> 'S -> 'T option
(** Attempts to update the focuses of the optic on the data. *)

(** {2 Setting focuses}

    *)

val can_set : ('S, 'F, 'G, 'T) optic -> 'G -> 'S -> bool
(** Determines whether the focuses of the optic on the data can be set. *)

val set : ('S, 'F, 'G, 'T) optic -> 'G -> 'S -> 'T
(** Sets the focuses of the optic on the data. Raises when cannot. *)

val set_default : ('S, 'F, 'G, 'S) optic -> 'G -> 'S -> 'S
(** Sets the focuses of the optic on the data. Returns input when cannot. *)

val set_opt : ('S, 'F, 'G, 'T) optic -> 'G -> 'S -> 'T option
(** Attempts to set the focuses of the optic on the data. *)

(** {2 Removing focuses}

    *)

val can_remove : ('S, 'F, 'G, 'T) optic -> 'S -> bool
(** Determines whether the focuses of the optic on the data can be removed. *)

val remove : ('S, 'F, 'G, 'T) optic -> 'S -> 'T
(** Removes the focuses of the optic on the data. Raises when cannot. *)

val remove_default : ('S, 'F, 'G, 'S) optic -> 'S -> 'S
(** Removes the focuses of the optic on the data. Returns input when cannot. *)

val remove_opt : ('S, 'F, 'G, 'T) optic -> 'S -> 'T option
(** Attempts to remove the focuses of the optic on the data. *)

(** {2 Dispersing focuses}

    *)

val disperse : ('S, 'F, 'G, 'T) optic -> 'G array -> 'S -> 'T
(** Injects array elements to the focuses or removes them. Raises when cannot. *)

val disperse_keep : ('S, 'F, 'F, 'T) optic -> 'F array -> 'S -> 'T
(** Injects array elements to the focuses or keeps them. Raises when cannot. *)

val parts_of : ('S, 'F, 'G, 'T) optic -> ('S, 'F array, 'G array, 'T) optic
(** Converts an optic to a lens focusing on an array of the focuses. *)

val parts_of_keep : ('S, 'F, 'F, 'T) optic -> ('S, 'F array, 'F array, 'T) optic
(** Converts an optic to a lens focusing on an array of the focuses. *)

(** {2 Transforms}

    *)

val transform : ('S, 'F, 'F, 'T) optic -> 'S -> 'T
(** [transform optic data] is equivalent to [over optic Fun.id data]. *)

(** {3 Transform ops}

    *)

val remove_op : ('S, 'F, 'G, 'T) optic
(** Signal removal of current focus. *)

val over_op : ('S -> 'T) -> ('S, 'F, 'G, 'T) optic
(** Update focus with given function. *)

val set_op : 'T -> ('S, 'F, 'G, 'T) optic
(** Update focus with given value. *)

(** {2 Combinators}

    *)

val choose : ('S -> ('S, 'F, 'G, 'T) optic) -> ('S, 'F, 'G, 'T) optic
(** A non-isomorphism computed from the focus. *)

val if_else :
  ('S -> bool) ->
  ('S, 'F, 'G, 'T) optic ->
  ('S, 'F, 'G, 'T) optic ->
  ('S, 'F, 'G, 'T) optic
(** A choice between two non-isomorphisms depending on the focus. *)

val or_else :
  ('S, 'F, 'G, 'T) optic -> ('S, 'F, 'G, 'T) optic -> ('S, 'F, 'G, 'T) optic
(** [or_else secondary primary] acts like the [primary] when it can view and
    otherwise like [secondary]. *)

val tuple'2 :
  ('S, 'L1, 'L2, 'S) optic ->
  ('S, 'R1, 'R2, 'T) optic ->
  ('S, 'L1 * 'R1, 'L2 * 'R2, 'T) optic
(** A lens focusing on a pair of focuses.  Given optics should be separable
    lenses. *)

val and_then :
  ('U, 'F, 'G, 'T) optic -> ('S, 'F, 'G, 'U) optic -> ('S, 'F, 'G, 'T) optic
(** [op_1 |> and_then op_2] first performs with [op_1] and then [op_2].
{[
    let 9 =
      L.transform (L.over_op (( + ) 2) |> L.and_then (L.over_op (( * ) 3))) 1
    let ("1", "2") =
      L.transform ((L.fst |> L.and_then L.snd) ^<< L.over_op Int.to_string)
        (1, 2)
]} *)

(** {2 Conditionals}

    The {!cond_of}, {!case}, and {!otherwise} combinators provide an alternative
    syntax for conditional optics.  In general,

    {[
      cond_of traversal
      @@ case predicate_1 optic_1
      @@ case predicate_2 optic_2
         ...
      @@ case predicate_n optic_n
      @@ otherwise optic
    ]}

    is equivalent to

    {[
      if_else (exists predicate_1 traversal) optic_1
      @@ if_else (exists predicate_2 traversal) optic_2
         ...
      @@ if_else (exists predicate_n traversal) optic_n
      @@ optic
    ]}

    using {!if_else} and {!exists}. *)

val cond_of : ('S, 'C, 'C, 'U) optic -> (('S, 'C, 'C, 'U) optic -> 'K) -> 'K
(** [cond_of traversal] starts a conditional optic definition where the
    following {!case}s use the [traversal] to extract targets for their
    predicates. *)

val case :
  ('C -> bool) ->
  ('S, 'F, 'G, 'T) optic ->
  (('S, 'C, 'C, 'U) optic -> ('S, 'F, 'G, 'T) optic) ->
  ('S, 'C, 'C, 'U) optic ->
  ('S, 'F, 'G, 'T) optic
(** [case predicate optic] continues a conditional optic definition with given
    [predicate] and consequent [optic]. *)

val otherwise :
  ('S, 'F, 'G, 'T) optic -> ('S, 'C, 'C, 'U) optic -> ('S, 'F, 'G, 'T) optic
(** [otherwise optic] ends a conditional optic definition and provides the
    default [optic] to use when none of the {!case}s match the focus. *)

(** {2 Primitives}

    *)

val zero : ('S, 'F, 'G, 'S) optic
(** An optic that never has a focus. *)

(** {2 Removal}

    *)

val removed_if : ('G -> bool) -> ('F, 'F, 'G, 'G) optic
(** An identity like optic that signals removal when written value matches
    predicate. *)

val as_removed : 'G -> ('F, 'F, 'G, 'G) optic
(** A prism like optic that signals removal when written with equal value. *)

val removed_as : 'G -> ('F, 'F, 'G, 'G) optic
(** A prism like optic that maps a removed focus to given value when written. *)

val removed_as_none : ('F, 'F, 'G, 'G option) optic
(** A prism like optic that maps a removed focus to [None] when written. *)

val none_as_removed : ('F, 'F, 'G option, 'G) optic
(** A prism like optic that maps [None] to a removed focus when written. *)

val removed_option : ('F option, 'F, 'G, 'G) optic
(** An optic that peels away [Some] on read and removes [None] on write. *)

(** {2 Prisms}

    *)

val accept : ('F -> bool) -> ('F, 'F, 'F, 'F) optic
(** A prism with a focus only when it passes the given predicate.
{[
    let [3; 1; 4; 1] =
      L.remove (L.List.elems ^<< L.accept (fun x -> 4 < x)) [5; 3; 1; 6; 4; 1]
]} *)

val reject : ('F -> bool) -> ('F, 'F, 'F, 'F) optic
(** A prism with a focus except when it passes the given predicate.
{[
    let [3; 1; 4; 1] =
      L.remove (L.List.elems ^<< L.reject (fun x -> x < 5)) [5; 3; 1; 6; 4; 1]
]} *)

(** {2 Isomorphisms}

    *)

val id : ('S, 'S, 'T, 'T) optic
(** The identity isomorphism. *)

val re : ('S, 'F, 'G, 'T) optic -> ('G, 'T, 'S, 'F) optic
(** Inverts the given optic. Raises when not given an invertible optic. *)

val reread : ('S -> 'F) -> ('S, 'F, 'T, 'T) optic
(** An isomorphism that maps the focus with given function when read. *)

val rewrite : ('G -> 'T) -> ('S, 'S, 'G, 'T) optic
(** An isomorphism that maps the focus with given function when written. *)

val normalize : ('S -> 'F) -> ('S, 'F, 'S, 'F) optic
(** An isomorphism that maps the focus with given function in both ways. *)

val subset : ('S -> bool) -> ('S, 'S option, 'T, 'T) optic
(** An isomorphism between values and values that pass the predicate. *)

val to_default : 'F -> ('F option, 'F, 'G, 'G) optic
(** An isomorphism that maps [None] to the given default value when read. *)

val of_default : 'G -> ('F, 'F, 'G, 'G option) optic
(** An isomorphism that maps the given default value to [None] when written. *)

val default : 'F -> ('F option, 'F, 'F, 'F option) optic
(** An isomorphism that maps [None] to the given default value when read and
    maps the default value to [None] when written. *)

val is_or : falsy:'F -> truthy:'F -> ('F, bool, bool, 'F) optic
(** An isomorphism between given values and booleans.  Only [truthy] maps to
    [true] when read. *)

val apply_at :
  ('S, 'Si, 'Fi, 'F) optic ->
  ('G, 'Gi, 'Ti, 'T) optic ->
  ('Si, 'Fi, 'Gi, 'Ti) optic ->
  ('S, 'F, 'G, 'T) optic
(** TODO *)

val iterate : ('S, 'S, 'T, 'T) optic -> ('S, 'S, 'T, 'T) optic
(** TODO *)

(** {2 Side-effects}

    *)

val before : ('S -> unit) -> ('S, 'S, 'T, 'T) optic
(** An identity like optic that performs a given side-effect when viewed. *)

val after : ?on_removed:(unit -> unit) -> ('T -> unit) -> ('S, 'S, 'T, 'T) optic
(** An identity like optic that performs given side-effects when written. *)

(** {2 Folds}

    *)

val fold : 'R -> ('R -> 'F -> 'R) -> ('S, 'F, 'G, 'T) optic -> 'S -> 'R
(** Folds over the focuses of the optic on the data.
{[
    let 8 = L.fold 0 (+) L.List.elems [3; 1; 4]
]} *)

val iter : ('S, 'F, 'G, 'T) optic -> ('F -> unit) -> 'S -> unit
(** Iterates over the focuses of the optic on the data. *)

val count : ('S, 'F, 'G, 'T) optic -> 'S -> int
(** Counts the number of focuses the optic has on the data. *)

val collect_with : ('F, 'FS) builder -> ('S, 'F, 'G, 'T) optic -> 'S -> 'FS
(** Collects values of the focuses of the optic on the data with the given
    builder. *)

val collect : ('S, 'F, 'G, 'T) optic -> 'S -> 'F list
(** Extracts a list of all the focuses of the optic on the data. *)

val exists : ('F -> bool) -> ('S, 'F, 'F, 'T) optic -> 'S -> bool
(** Determines whether any focus of the optic on the data satisfy the
    predicate. *)

val forall : ('F -> bool) -> ('S, 'F, 'F, 'T) optic -> 'S -> bool
(** Determines whether all focuces of the optic on the data satisfy the
    predicate. *)

val disjunction : ('S, bool, bool, 'T) optic -> 'S -> bool
(** [disjunction optic data] is equivalent to [exists Fun.id optic data]. *)

val conjunction : ('S, bool, bool, 'T) optic -> 'S -> bool
(** [conjunction optic data] is equivalent to [forall Fun.id optic data]. *)

val concat : string -> ('S, string, 'G, 'T) optic -> 'S -> string
(** Concatenates all of the focuses of the optic on the data with given
    separator. *)

(** {2 Pairs}

    *)

val fst : ('L1 * 'R, 'L1, 'L2, 'L2 * 'R) optic
(** A lens focusing on the first element of a pair. *)

val snd : ('L * 'R1, 'R1, 'R2, 'L * 'R2) optic
(** A lens focusing on the second element of a pair. *)

val iso_pair :
  ('SL, 'FL, 'GL, 'TL) optic ->
  ('SR, 'FR, 'GR, 'TR) optic ->
  ('SL * 'SR, 'FL * 'FR, 'GL * 'GR, 'TL * 'TR) optic
(** An isomorphism between pairs. *)

(** {2 Tuples}

    *)

val elem_1_of_2 : ('F * 'X2, 'F, 'G, 'G * 'X2) optic
val elem_2_of_2 : ('X1 * 'F, 'F, 'G, 'X1 * 'G) optic
val elem_1_of_3 : ('F * 'X2 * 'X3, 'F, 'G, 'G * 'X2 * 'X3) optic
val elem_2_of_3 : ('X1 * 'F * 'X3, 'F, 'G, 'X1 * 'G * 'X3) optic
val elem_3_of_3 : ('X1 * 'X2 * 'F, 'F, 'G, 'X1 * 'X2 * 'G) optic
val elem_1_of_4 : ('F * 'X2 * 'X3 * 'X4, 'F, 'G, 'G * 'X2 * 'X3 * 'X4) optic
val elem_2_of_4 : ('X1 * 'F * 'X3 * 'X4, 'F, 'G, 'X1 * 'G * 'X3 * 'X4) optic
val elem_3_of_4 : ('X1 * 'X2 * 'F * 'X4, 'F, 'G, 'X1 * 'X2 * 'G * 'X4) optic
val elem_4_of_4 : ('X1 * 'X2 * 'X3 * 'F, 'F, 'G, 'X1 * 'X2 * 'X3 * 'G) optic

(** {2 Numerics}

    *)

val truncate : (float, int, int, float) optic
(** An isomorphism between integers and floats. *)

(** {2 Stdlib data types}

    *)

module Array : sig
  open Array

  val map : ('S, 'F, 'G, 'T) optic -> ('S t, 'F t, 'G t, 'T t) optic
  (** An isomorphism between arrays.
{[
      let [|4; 2|] = L.view (L.Array.map (L.accept ((<) 0))) [|4; -3; 2|]
]} *)

  val elems : ('F t, 'F, 'G, 'G t) optic
  (** A traversal over the removable elements of an array. *)

  val as_list : ('F t, 'F list, 'G list, 'G t) optic
  (** Isomorphism between arrays and lists. *)

  val as_option : ('F t, 'F option, 'G option, 'G t) optic
  (** Isomorphism between singleton arrays and options. *)

  val at : int -> ('F t, 'F, 'F, 'F t) optic
  (** A traversal focusing on a removable element at given index of a list.
{[
    let [|3; 1; 4|] = L.over (L.Array.at 2) ((+) 1) [|3; 1; 3|]
    let [|4; 2|] = L.remove (L.Array.at 2) [|4; 2; 1|]
]} *)

  val at_ref : int ref -> ('F t, 'F, 'F, 'F t) optic
  (** A traversal focusing on a removable element at a mutable index of a list. *)
end

[%%if (4, 12, 0) <= ocaml_version]

module Either : sig
  open Either

  val map :
    ('SL, 'FL, 'GL, 'TL) optic ->
    ('SR, 'FR, 'GR, 'TR) optic ->
    (('SL, 'SR) t, ('FL, 'FR) t, ('GL, 'GR) t, ('TL, 'TR) t) optic
  (** An isomorphism between eithers. *)

  val on_left : (('F, 'S) t, 'F, 'G, ('G, 'S) t) optic
  (** A prism that focuses on [Left]. *)

  val on_right : (('S, 'F) t, 'F, 'G, ('S, 'G) t) optic
  (** A prism that focuses on [Right]. *)
end

[%%endif]

module List : sig
  open List

  val hd : ('F t, 'F, 'F, 'F t) optic
  (** A prism that focuses on the first removable element of a list.
{[
      let None = L.view_opt L.List.hd []
      let 3 = L.view L.List.hd [3; 1; 4]
      let [-3; 1; 4] = L.over L.List.hd (~-) [3; 1; 4]
      let [1; 4] = L.remove L.List.hd [3; 1; 4]
]} *)

  val tl : ('F t, 'F t, 'F t, 'F t) optic
  (** A prism that focuses on the removable tail of a list.
{[
      let None = L.view_opt L.List.tl []
      let [1; 4] = L.view L.List.tl [3; 1; 4]
      let [3; 4; 1] = L.over L.List.tl List.rev [3; 1; 4]
      let [3] = L.remove L.List.tl [3; 1; 4]
]} *)

  val map : ('S, 'F, 'G, 'T) optic -> ('S t, 'F t, 'G t, 'T t) optic
  (** An isomorphism between lists. *)

  val elems : ('F t, 'F, 'G, 'G t) optic
  (** A traversal over the removable elements of a list. *)

  val as_array : ('F t, 'F array, 'G array, 'G t) optic
  (** Isomorphism between lists and arrays. *)

  val as_option : ('F t, 'F option, 'G option, 'G t) optic
  (** Isomorphism between singleton lists and options.
{[
      let None = L.view L.List.as_option ["more"; "than"; "one"]
      let Some "exactly one" = L.view L.List.as_option ["exactly one"]
      let None = L.view L.List.as_option []
]} *)

  val rev : ('F t, 'F t, 'G t, 'G t) optic
  (** An isomorphism whose focus is reverse of the list.
{[
      let [3; 1; 4] = L.over L.List.rev (List.cons 4) [3; 1]
]} *)

  val partition : ('F -> bool) -> ('F t, 'F t * 'F t, 'G t * 'G t, 'G t) optic
  (** An isomorphism that partitions a list into sublists of passes and fails.
{[
      let ([1; 2], [3; 4; 5]) =
        L.view (L.List.partition (fun x -> x <= 2)) [3; 1; 4; 2; 5]
]} *)

  val filter : ('F -> bool) -> ('F t, 'F t, 'F t, 'F t) optic
  (** A lens that focuses on sublist of passes.
{[
      let [1; 2] = L.view (L.List.filter (fun x -> x <= 2)) [3; 1; 4; 2; 5]
]} *)

  val reject : ('F -> bool) -> ('F t, 'F t, 'F t, 'F t) optic
  (** A lens that focuses on sublist of fails.
{[
      let [3; 4; 5] = L.view (L.List.reject (fun x -> x <= 2)) [3; 1; 4; 2; 5]
]} *)
end

(**
{[
    module StringMap = struct
      include Map.Make (String)
      include L.Map.Make (Map.Make (String))
    end
    let a_string_map = L.review StringMap.as_assoc [("foo", 101); ("bar", 42)]
]} *)
module Map : sig
  module Make (M : Map.S) : sig
    val at : M.key -> ('a M.t, 'a, 'a, 'a M.t) optic
    (** A traversal focusing on removable value of given key in a map.
{[
        let 101 = L.view (StringMap.at "foo") a_string_map
        let true =
          StringMap.equal ( = )
            (L.review StringMap.as_assoc [("bar", 42)])
            (L.remove (StringMap.at "foo") a_string_map)
]} *)

    val as_assoc : ('a M.t, (M.key * 'a) list, (M.key * 'b) list, 'b M.t) optic
    (** Isomorphism between maps and association lists. *)

    val elems : ('a M.t, 'a, 'a, 'a M.t) optic
    (** Traversal over the removable elements of a map. *)
  end
end

module Option : sig
  open Option

  val map : ('S, 'F, 'G, 'T) optic -> ('S t, 'F t, 'G t, 'T t) optic
  (** An isomorphism between options. *)

  val elems : ('F t, 'F, 'G, 'G t) optic
  (** A prism over the removable optional value.
{[
      let Some 3 = L.over L.Option.elems ((+) 2) (Some 1)
      let None = L.remove L.Option.elems (Some 1)
      let Some 1 = L.review L.Option.elems 1
]} *)
end

module Result : sig
  open Result

  val map :
    ('SO, 'FO, 'GO, 'TO) optic ->
    ('SE, 'FE, 'GE, 'TE) optic ->
    (('SO, 'SE) t, ('FO, 'FE) t, ('GO, 'GE) t, ('TO, 'TE) t) optic
  (** An isomorphism between results. *)

  val on_ok : (('F, 'S) t, 'F, 'G, ('G, 'S) t) optic
  (** A prism that focuses on [Ok]. *)

  val on_error : (('S, 'F) t, 'F, 'G, ('S, 'G) t) optic
  (** A prism that focuses on [Error]. *)
end

module String : sig
  open String

  [%%if (4, 13, 0) <= ocaml_version]

  val ends_with : t -> (t, t, t, t) optic
  (** A prism that focuses on the prefix of a string that ends with the given
      suffix. *)

  [%%endif]

  val escaped : (t, t, t, t) optic
  (** An isomorphism that escapes on read and unescapes on write.
{[
      let "'\\t' vs ' '" = L.view L.String.escaped "'\t' vs ' '"
]} *)

  val split_on_char : char -> (t, t list, t list, t) optic
  (** An isomorphism between separated string and list of strings.
{[
      let "One:Two:Three" =
        L.over (L.String.split_on_char ':' ^<< L.List.elems)
          String.capitalize_ascii
          "one:two:three"
]} *)

  [%%if (4, 13, 0) <= ocaml_version]

  val starts_with : t -> (t, t, t, t) optic
  (** A prism that focuses on the suffix of a string that starts with the given
      prefix. *)

  [%%endif]

  val sub : int -> int -> (t, t, t, t) optic
  (** [sub pos len] is a lens that focuses on the substring that starts at
      specified [pos]ition and has given [len]gth.
{[
      let "Loko" = L.set (L.String.sub 1 3) "ok" "Lasso"
]} *)

  val trim : (t, t, t, t) optic
  (** An isomorphism that [trim]s the string in both directions. *)

  val unescaped : (t, t, t, t) optic
  (** An isomorphism that unescapes on read and escapes on write. *)
end
