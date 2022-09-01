(** Lower-Kinded Optics

    Typically one either refers to the library primitives through the [Loko]
    module name or, as done in this documentation, uses an abbreviation such as
    {[
      module L = Loko
    ]}
    to keep code more succinct.

    [Loko] uses ordinary function composition to compose optics and for
    convenience, the {!Infix} submodule
    {[
      open L.Infix
    ]}
    provides the {{!Infix.(%)} [%]} operator for that purpose. *)

(** {2 Auxiliary abstractions}

    The abstractions in this section serve auxiliary roles for the definition of
    optics. *)

module Infix : sig
  (** Infix operators for convenient use of optics. *)

  val ( % ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** [(f % g) x] is [f (g x)], i.e. [%] is function composition. *)
end

module Iterator : sig
  (** Imperative iterator. *)

  exception Done
  (** Raised by [next] without backtrace after all the elements have been
      returned. *)

  (** An iterator extracts elements of type ['x] one-by-one from a collection of
      type ['xs]. *)
  type ('xs, 'x) t =
    | T : { start : 'xs -> 'iterator; next : 'iterator -> 'x } -> ('xs, 'x) t
end

module Builder : sig
  (** Imperative builder. *)

  (** A builder accumulates elements of type ['y] one-by-one to produce a
      collection of type ['ys]. *)
  type ('y, 'ys) t =
    | T : {
        empty : unit -> 'builder;
        add : 'builder -> 'y -> unit;
        finish : 'builder -> 'ys;
      }
        -> ('y, 'ys) t
end

(** {2 Optic types} *)

type (-'S, +'T) pipe
(** The [pipe] type constructor hides most of the implementation details of
    optics and basically represents a function that takes values of type ['S] as
    input and produces values of type ['T] as output. *)

type (-'S, +'F, -'G, +'T) t = ('F, 'G) pipe -> ('S, 'T) pipe
(** Optics are functions over {!pipe}s and are composable with normal function
    composition. *)

type ('S, 'F, 'G, 'T) optic := ('S, 'F, 'G, 'T) t
(** Erased type alias for optic. *)

(** {2 Optic constructors} *)

val iso : ('S -> 'F) -> ('G -> 'T) -> ('S, 'F, 'G, 'T) t
(** Defines a new invertible isomorphism from a pair of conversion functions.
    {[
      # let plus n = L.iso (( + ) n) (Fun.flip ( - ) n)
      val plus : int -> (int, int, int, int) L.t = <fun>

      # L.view (plus 1) 2
      - : int = 3

      # L.review (plus 2) 3
      - : int = 1

      # L.view (L.re (plus 2)) 3
      - : int = 1
    ]} *)

val lens : ('S -> 'F) -> ('G -> 'S -> 'T) -> ('S, 'F, 'G, 'T) t
(** Defines a new lens from a getter and setter. *)

val partial_lens :
  ('S -> 'F option) -> ('G -> 'S -> 'T) -> ('S -> 'T) -> ('S, 'F, 'G, 'T) t
(** Defines a new partial lens from a getter, setter, and remover. *)

val getter : ('S -> 'F) -> ('S, 'F, 'G, 'S) t
(** Defines a new getter. *)

val fold_lens :
  (('S, 'E, 'G, 'T) t -> 'S -> 'F) -> ('S, 'E, 'G, 'T) t -> ('S, 'F, 'G, 'T) t
(** [fold_lens fold traversal] defines a lens out of a suitable [fold] and a
    [traversal]. *)

val prism : ('S -> ('T, 'F) Either.t) -> ('G -> 'T) -> ('S, 'F, 'G, 'T) t
(** Defines a new invertible prism from a constructor-destructor pair.
    {[
      # let ends_with suffix =
          L.prism
            (fun string ->
              if Filename.check_suffix string suffix then
                Right (Filename.chop_suffix string suffix)
              else Left string)
            (fun prefix -> prefix ^ suffix)
      val ends_with : string -> (string, string, string, string) L.t = <fun>

      # L.view (ends_with ".ml") "Loko.ml"
      - : string = "Loko"

      # L.set (ends_with ".ml") "Loco" "Loko.ml"
      - : string = "Loco.ml"

      # L.can_view (ends_with ".ml") "Loko.mli"
      - : bool = false

      # L.set (ends_with ".ml") "Loco" "Loko.mli"
      - : string = "Loko.mli"
    ]} *)

val traversal : ('S, 'F) Iterator.t -> ('G, 'T) Builder.t -> ('S, 'F, 'G, 'T) t
(** Defines a new traversal from an iterator-builder pair. *)

val branch'2 :
  ('F1, 'F, 'G, 'G1) t ->
  ('F2, 'F, 'G, 'G2) t ->
  ('F1 * 'F2, 'F, 'G, 'G1 * 'G2) t
(** Defines a new traversal over the elements of a 2-tuple. *)

val branch'3 :
  ('F1, 'F, 'G, 'G1) t ->
  ('F2, 'F, 'G, 'G2) t ->
  ('F3, 'F, 'G, 'G3) t ->
  ('F1 * 'F2 * 'F3, 'F, 'G, 'G1 * 'G2 * 'G3) t
(** Defines a new traversal over the elements of a 3-tuple. *)

val branch'4 :
  ('F1, 'F, 'G, 'G1) t ->
  ('F2, 'F, 'G, 'G2) t ->
  ('F3, 'F, 'G, 'G3) t ->
  ('F4, 'F, 'G, 'G4) t ->
  ('F1 * 'F2 * 'F3 * 'F4, 'F, 'G, 'G1 * 'G2 * 'G3 * 'G4) t
(** Defines a new traversal over the elements of a 4-tuple. *)

(** {2 Viewing a focus} *)

val can_view : ('S, 'F, 'G, 'T) t -> 'S -> bool
(** Determines whether the optic has a focus on the data. *)

val view : ('S, 'F, 'G, 'T) t -> 'S -> 'F
(** Extracts the first focus of the optic on the data. Raises when there is no
    focus. *)

val view_opt : ('S, 'F, 'G, 'T) t -> 'S -> 'F option
(** Attempts to extract the first focus of the optic on the data. *)

(** {2 Reviewing through invertible optics} *)

val can_review : ('S, 'F, 'G, 'T) t -> 'G -> bool
(** Determines whether the optic has an inverse focus on the data. *)

val review : ('S, 'F, 'G, 'T) t -> 'G -> 'T
(** Views through isomorphism in inverse direction. Raises when the optic is not
    reversible or doesn't have inverse focus on the data. *)

val review_opt : ('S, 'F, 'G, 'T) t -> 'G -> 'T option
(** Attempts to extract the inverse focus of the optic on the data. WARNING:
    This will raise when the optic is not reversible. *)

(** {2 Mapping over focuses} *)

val can_over : ('S, 'F, 'G, 'T) t -> ('F -> 'G) -> 'S -> bool
(** Determines whether the focuses of the optic on the data can be updated. *)

val over : ('S, 'F, 'G, 'T) t -> ('F -> 'G) -> 'S -> 'T
(** Updates the focuses of the optic on the data. Raises when cannot. *)

val over_default : ('S, 'F, 'G, 'S) t -> ('F -> 'G) -> 'S -> 'S
(** Updates the focuses of the optic on the data. Returns input when cannot. *)

val over_opt : ('S, 'F, 'G, 'T) t -> ('F -> 'G) -> 'S -> 'T option
(** Attempts to update the focuses of the optic on the data. *)

(** {2 Setting focuses} *)

val can_set : ('S, 'F, 'G, 'T) t -> 'G -> 'S -> bool
(** Determines whether the focuses of the optic on the data can be set. *)

val set : ('S, 'F, 'G, 'T) t -> 'G -> 'S -> 'T
(** Sets the focuses of the optic on the data. Raises when cannot. *)

val set_default : ('S, 'F, 'G, 'S) t -> 'G -> 'S -> 'S
(** Sets the focuses of the optic on the data. Returns input when cannot. *)

val set_opt : ('S, 'F, 'G, 'T) t -> 'G -> 'S -> 'T option
(** Attempts to set the focuses of the optic on the data. *)

(** {2 Removing focuses} *)

val can_remove : ('S, 'F, 'G, 'T) t -> 'S -> bool
(** Determines whether the focuses of the optic on the data can be removed. *)

val remove : ('S, 'F, 'G, 'T) t -> 'S -> 'T
(** Removes the focuses of the optic on the data. Raises when cannot. *)

val remove_default : ('S, 'F, 'G, 'S) t -> 'S -> 'S
(** Removes the focuses of the optic on the data. Returns input when cannot. *)

val remove_opt : ('S, 'F, 'G, 'T) t -> 'S -> 'T option
(** Attempts to remove the focuses of the optic on the data. *)

(** {2 Dispersing focuses} *)

val disperse : ('S, 'F, 'G, 'T) t -> 'G array -> 'S -> 'T
(** Injects array elements to the focuses or removes them. Raises when cannot.
*)

val disperse_keep : ('S, 'F, 'F, 'T) t -> 'F array -> 'S -> 'T
(** Injects array elements to the focuses or keeps them. Raises when cannot. *)

val parts_of : ('S, 'F, 'G, 'T) t -> ('S, 'F array, 'G array, 'T) t
(** Converts an optic to a lens focusing on an array of the focuses. *)

val parts_of_keep : ('S, 'F, 'F, 'T) t -> ('S, 'F array, 'F array, 'T) t
(** Converts an optic to a lens focusing on an array of the focuses. *)

(** {2 Transforms} *)

val transform : ('S, 'F, 'F, 'T) t -> 'S -> 'T
(** [transform optic data] is equivalent to [over optic Fun.id data]. *)

(** {3 Transform ops} *)

val remove_op : ('S, 'F, 'G, 'T) t
(** Signal removal of current focus. *)

val over_op : ('S -> 'T) -> ('S, 'F, 'G, 'T) t
(** Update focus with given function. *)

val set_op : 'T -> ('S, 'F, 'G, 'T) t
(** Update focus with given value. *)

(** {2 Combinators} *)

val choose : ('S -> ('S, 'F, 'G, 'T) t) -> ('S, 'F, 'G, 'T) t
(** A non-isomorphism computed from the focus. *)

val if_else :
  ('S -> bool) -> ('S, 'F, 'G, 'T) t -> ('S, 'F, 'G, 'T) t -> ('S, 'F, 'G, 'T) t
(** A choice between two non-isomorphisms depending on the focus. *)

val or_else : ('S, 'F, 'G, 'T) t -> ('S, 'F, 'G, 'T) t -> ('S, 'F, 'G, 'T) t
(** [or_else secondary primary] acts like the [primary] when it can view and
    otherwise like [secondary]. *)

val tuple'2 :
  ('S, 'L1, 'L2, 'S) t ->
  ('S, 'R1, 'R2, 'T) t ->
  ('S, 'L1 * 'R1, 'L2 * 'R2, 'T) t
(** A lens focusing on a pair of focuses. Given optics should be separable
    lenses. *)

val and_then : ('U, 'F, 'G, 'T) t -> ('S, 'F, 'G, 'U) t -> ('S, 'F, 'G, 'T) t
(** [op_1 |> and_then op_2] first performs with [op_1] and then [op_2].
    {[
      # L.transform (L.over_op (( + ) 2) |> L.and_then (L.over_op (( * ) 3))) 1
      - : int = 9

      # L.transform
          ((L.fst |> L.and_then L.snd) % L.over_op Int.to_string)
          (1, 2)
      - : string * string = ("1", "2")
    ]} *)

(** {2 Conditionals}

    The {!cond_of}, {!case}, and {!otherwise} combinators provide an alternative
    syntax for conditional optics. In general,

    {@ocaml skip[
      cond_of traversal
      @@ case predicate_1 optic_1
      @@ case predicate_2 optic_2
         ...
      @@ case predicate_n optic_n
      @@ otherwise optic
    ]}

    is equivalent to

    {@ocaml skip[
      if_else (exists predicate_1 traversal) t_1
      @@ if_else (exists predicate_2 traversal) optic_2
         ...
      @@ if_else (exists predicate_n traversal) optic_n
      @@ optic
    ]}

    using {!if_else} and {!exists}. *)

val cond_of : ('S, 'C, 'C, 'U) t -> (('S, 'C, 'C, 'U) t -> 'K) -> 'K
(** [cond_of traversal] starts a conditional optic definition where the
    following {!case}s use the [traversal] to extract targets for their
    predicates. *)

val case :
  ('C -> bool) ->
  ('S, 'F, 'G, 'T) t ->
  (('S, 'C, 'C, 'U) t -> ('S, 'F, 'G, 'T) t) ->
  ('S, 'C, 'C, 'U) t ->
  ('S, 'F, 'G, 'T) t
(** [case predicate optic] continues a conditional optic definition with given
    [predicate] and consequent [optic]. *)

val otherwise : ('S, 'F, 'G, 'T) t -> ('S, 'C, 'C, 'U) t -> ('S, 'F, 'G, 'T) t
(** [otherwise optic] ends a conditional optic definition and provides the
    default [optic] to use when none of the {!case}s match the focus. *)

(** {2 Primitives} *)

val zero : ('S, 'F, 'G, 'S) t
(** An optic that never has a focus. *)

(** {2 Removal} *)

val removed_if : ('G -> bool) -> ('F, 'F, 'G, 'G) t
(** An identity like optic that signals removal when written value matches
    predicate. *)

val as_removed : 'G -> ('F, 'F, 'G, 'G) t
(** A prism like optic that signals removal when written with equal value. *)

val removed_as : 'G -> ('F, 'F, 'G, 'G) t
(** A prism like optic that maps a removed focus to given value when written. *)

val removed_as_none : ('F, 'F, 'G, 'G option) t
(** A prism like optic that maps a removed focus to [None] when written. *)

val none_as_removed : ('F, 'F, 'G option, 'G) t
(** A prism like optic that maps [None] to a removed focus when written. *)

val removed_option : ('F option, 'F, 'G, 'G) t
(** An optic that peels away [Some] on read and removes [None] on write. *)

(** {2 Prisms} *)

val accept : ('F -> bool) -> ('F, 'F, 'F, 'F) t
(** A prism with a focus only when it passes the given predicate.
    {@ocaml version>=5.0.0[
      # L.remove
          (L.List.elems % L.accept (fun x -> 4 < x))
          [ 5; 3; 1; 6; 4; 1 ]
      - : int list = [3; 1; 4; 1]
    ]} *)

val reject : ('F -> bool) -> ('F, 'F, 'F, 'F) t
(** A prism with a focus except when it passes the given predicate.
    {@ocaml version>=5.0.0[
      # L.remove
          (L.List.elems % L.reject (fun x -> x < 5))
          [ 5; 3; 1; 6; 4; 1 ]
      - : int list = [3; 1; 4; 1]
    ]} *)

(** {2 Isomorphisms} *)

val id : ('S, 'S, 'T, 'T) t
(** The identity isomorphism. *)

val re : ('S, 'F, 'G, 'T) t -> ('G, 'T, 'S, 'F) t
(** Inverts the given optic. Raises when not given an invertible optic. *)

val reread : ('S -> 'F) -> ('S, 'F, 'T, 'T) t
(** An isomorphism that maps the focus with given function when read. *)

val rewrite : ('G -> 'T) -> ('S, 'S, 'G, 'T) t
(** An isomorphism that maps the focus with given function when written. *)

val normalize : ('S -> 'F) -> ('S, 'F, 'S, 'F) t
(** An isomorphism that maps the focus with given function in both ways. *)

val subset : ('S -> bool) -> ('S, 'S option, 'T, 'T) t
(** An isomorphism between values and values that pass the predicate. *)

val to_default : 'F -> ('F option, 'F, 'G, 'G) t
(** An isomorphism that maps [None] to the given default value when read. *)

val of_default : 'G -> ('F, 'F, 'G, 'G option) t
(** An isomorphism that maps the given default value to [None] when written. *)

val default : 'F -> ('F option, 'F, 'F, 'F option) t
(** An isomorphism that maps [None] to the given default value when read and
    maps the default value to [None] when written. *)

val is_or : falsy:'F -> truthy:'F -> ('F, bool, bool, 'F) t
(** An isomorphism between given values and booleans. Only [truthy] maps to
    [true] when read. *)

val apply_at :
  ('S, 'Si, 'Fi, 'F) t ->
  ('G, 'Gi, 'Ti, 'T) t ->
  ('Si, 'Fi, 'Gi, 'Ti) t ->
  ('S, 'F, 'G, 'T) t
(** TODO *)

val iterate : ('S, 'S, 'T, 'T) t -> ('S, 'S, 'T, 'T) t
(** TODO *)

(** {2 Side-effects} *)

val before : ('S -> unit) -> ('S, 'S, 'T, 'T) t
(** An identity like optic that performs a given side-effect when viewed. *)

val after : ?on_removed:(unit -> unit) -> ('T -> unit) -> ('S, 'S, 'T, 'T) t
(** An identity like optic that performs given side-effects when written. *)

(** {2 Folds} *)

val fold : 'R -> ('R -> 'F -> 'R) -> ('S, 'F, 'G, 'T) t -> 'S -> 'R
(** Folds over the focuses of the optic on the data.
    {[
      # L.fold 0 ( + ) L.List.elems [ 3; 1; 4 ]
      - : int = 8
    ]} *)

val iter : ('S, 'F, 'G, 'T) t -> ('F -> unit) -> 'S -> unit
(** Iterates over the focuses of the optic on the data. *)

val count : ('S, 'F, 'G, 'T) t -> 'S -> int
(** Counts the number of focuses the optic has on the data. *)

val collect_with : ('F, 'FS) Builder.t -> ('S, 'F, 'G, 'T) t -> 'S -> 'FS
(** Collects values of the focuses of the optic on the data with the given
    builder. *)

val collect : ('S, 'F, 'G, 'T) t -> 'S -> 'F list
(** Extracts a list of all the focuses of the optic on the data. *)

val exists : ('F -> bool) -> ('S, 'F, 'F, 'T) t -> 'S -> bool
(** Determines whether any focus of the optic on the data satisfy the predicate.
*)

val forall : ('F -> bool) -> ('S, 'F, 'F, 'T) t -> 'S -> bool
(** Determines whether all focuces of the optic on the data satisfy the
    predicate. *)

val disjunction : ('S, bool, bool, 'T) t -> 'S -> bool
(** [disjunction optic data] is equivalent to [exists Fun.id optic data]. *)

val conjunction : ('S, bool, bool, 'T) t -> 'S -> bool
(** [conjunction optic data] is equivalent to [forall Fun.id optic data]. *)

val concat : string -> ('S, string, 'G, 'T) t -> 'S -> string
(** Concatenates all of the focuses of the optic on the data with given
    separator. *)

(** {2 Pairs} *)

val fst : ('L1 * 'R, 'L1, 'L2, 'L2 * 'R) t
(** A lens focusing on the first element of a pair. *)

val snd : ('L * 'R1, 'R1, 'R2, 'L * 'R2) t
(** A lens focusing on the second element of a pair. *)

val iso_pair :
  ('SL, 'FL, 'GL, 'TL) t ->
  ('SR, 'FR, 'GR, 'TR) t ->
  ('SL * 'SR, 'FL * 'FR, 'GL * 'GR, 'TL * 'TR) t
(** An isomorphism between pairs. *)

(** {2 Tuples} *)

val elem_1_of_2 : ('F * 'X2, 'F, 'G, 'G * 'X2) t
val elem_2_of_2 : ('X1 * 'F, 'F, 'G, 'X1 * 'G) t
val elem_1_of_3 : ('F * 'X2 * 'X3, 'F, 'G, 'G * 'X2 * 'X3) t
val elem_2_of_3 : ('X1 * 'F * 'X3, 'F, 'G, 'X1 * 'G * 'X3) t
val elem_3_of_3 : ('X1 * 'X2 * 'F, 'F, 'G, 'X1 * 'X2 * 'G) t
val elem_1_of_4 : ('F * 'X2 * 'X3 * 'X4, 'F, 'G, 'G * 'X2 * 'X3 * 'X4) t
val elem_2_of_4 : ('X1 * 'F * 'X3 * 'X4, 'F, 'G, 'X1 * 'G * 'X3 * 'X4) t
val elem_3_of_4 : ('X1 * 'X2 * 'F * 'X4, 'F, 'G, 'X1 * 'X2 * 'G * 'X4) t
val elem_4_of_4 : ('X1 * 'X2 * 'X3 * 'F, 'F, 'G, 'X1 * 'X2 * 'X3 * 'G) t

(** {2 Numerics} *)

val truncate : (float, int, int, float) t
(** An isomorphism between integers and floats. *)

(** {2 Stdlib data types} *)

module Array : sig
  (** Optics for [Array]. *)

  open Array

  val iterator : ('a t, 'a) Iterator.t
  (** Array iterator. *)

  val builder : ('a, 'a t) Builder.t
  (** Array builder. *)

  val map : ('S, 'F, 'G, 'T) optic -> ('S t, 'F t, 'G t, 'T t) optic
  (** An isomorphism between arrays.
      {[
        # L.view (L.Array.map (L.accept (( < ) 0))) [| 4; -3; 2 |]
        - : int array = [|4; 2|]
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
        # L.over (L.Array.at 2) (( + ) 1) [| 3; 1; 3 |]
        - : int array = [|3; 1; 4|]

        # L.remove (L.Array.at 2) [| 4; 2; 1 |]
        - : int array = [|4; 2|]
      ]} *)

  val at_ref : int ref -> ('F t, 'F, 'F, 'F t) optic
  (** A traversal focusing on a removable element at a mutable index of a list.
  *)
end

module Either : sig
  (** Optics for [Either]. *)

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

module List : sig
  (** Optics for [List]. *)

  open List

  val iterator : ('a t, 'a) Iterator.t
  (** List iterator. *)

  val builder : ('a, 'a t) Builder.t
  (** List builder. *)

  val hd : ('F t, 'F, 'F, 'F t) optic
  (** A prism that focuses on the first removable element of a list.
      {@ocaml version>=5.0.0[
        # L.view_opt L.List.hd []
        - : 'a option = None

        # L.view L.List.hd [ 3; 1; 4 ]
        - : int = 3

        # L.over L.List.hd ( ~- ) [ 3; 1; 4 ]
        - : int list = [-3; 1; 4]

        # L.remove L.List.hd [ 3; 1; 4 ]
        - : int list = [1; 4]
      ]} *)

  val tl : ('F t, 'F t, 'F t, 'F t) optic
  (** A prism that focuses on the removable tail of a list.
      {@ocaml version>=5.0.0[
        # L.view_opt L.List.tl []
        - : 'a list option = None

        # L.view L.List.tl [ 3; 1; 4 ]
        - : int list = [1; 4]

        # L.over L.List.tl List.rev [ 3; 1; 4 ]
        - : int list = [3; 4; 1]

        # L.remove L.List.tl [ 3; 1; 4 ]
        - : int list = [3]
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
        # L.view L.List.as_option [ "more"; "than"; "one" ]
        - : string option = None

        # L.view L.List.as_option [ "exactly one" ]
        - : string option = Some "exactly one"

        # L.view L.List.as_option []
        - : 'a option = None
      ]} *)

  val as_rev : ('F t, 'F t, 'G t, 'G t) optic
  (** An isomorphism whose focus is reverse of the list.
      {@ocaml version>=5.0.0[
        # L.over L.List.as_rev (List.cons 4) [ 3; 1 ]
        - : int list = [3; 1; 4]
      ]} *)

  val partition : ('F -> bool) -> ('F t, 'F t * 'F t, 'G t * 'G t, 'G t) optic
  (** An isomorphism that partitions a list into sublists of passes and fails.
      {@ocaml version>=5.0.0[
        # L.view (L.List.partition (fun x -> x <= 2)) [ 3; 1; 4; 2; 5 ]
        - : int list * int list = ([1; 2], [3; 4; 5])
      ]} *)

  val filter : ('F -> bool) -> ('F t, 'F t, 'F t, 'F t) optic
  (** A lens that focuses on sublist of passes.
      {@ocaml version>=5.0.0[
        # L.view (L.List.filter (fun x -> x <= 2)) [ 3; 1; 4; 2; 5 ]
        - : int list = [1; 2]
      ]} *)

  val reject : ('F -> bool) -> ('F t, 'F t, 'F t, 'F t) optic
  (** A lens that focuses on sublist of fails.
      {@ocaml version>=5.0.0[
        # L.view (L.List.reject (fun x -> x <= 2)) [ 3; 1; 4; 2; 5 ]
        - : int list = [3; 4; 5]
      ]} *)
end

module Map : sig
  (** Optics for [Map]. *)

  (** {[
        module StringMap = struct
          include Map.Make (String)
          include L.Map.Make (Map.Make (String))
        end

        let a_string_map =
          L.review StringMap.as_assoc [ ("foo", 101); ("bar", 42) ]
      ]} *)

  module Make (M : Map.S) : sig
    val at : M.key -> ('a M.t, 'a, 'a, 'a M.t) optic
    (** A traversal focusing on removable value of given key in a map.
        {[
          # L.view (StringMap.at "foo") a_string_map
          - : int = 101

          # StringMap.equal ( = )
              (L.review StringMap.as_assoc [ ("bar", 42) ])
              (L.remove (StringMap.at "foo") a_string_map)
          - : bool = true
        ]} *)

    val as_assoc : ('a M.t, (M.key * 'a) list, (M.key * 'b) list, 'b M.t) optic
    (** Isomorphism between maps and association lists. *)

    val elems : ('a M.t, 'a, 'a, 'a M.t) optic
    (** Traversal over the removable elements of a map. *)
  end
end

module Option : sig
  (** Optics for [Option]. *)

  open Option

  val map : ('S, 'F, 'G, 'T) optic -> ('S t, 'F t, 'G t, 'T t) optic
  (** An isomorphism between options. *)

  val elems : ('F t, 'F, 'G, 'G t) optic
  (** A prism over the removable optional value.
      {[
        # L.over L.Option.elems (( + ) 2) (Some 1)
        - : int option = Option.Some 3

        # L.remove L.Option.elems (Some 1)
        - : 'a option = Option.None

        # L.review L.Option.elems 1
        - : int option = Option.Some 1
      ]} *)
end

module Result : sig
  (** Optics for [Result]. *)

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
  (** Optics for [String]. *)

  open String

  val ends_with : t -> (t, t, t, t) optic
  (** A prism that focuses on the prefix of a string that ends with the given
      suffix. *)

  val as_escaped : (t, t, t, t) optic
  (** An isomorphism that escapes on read and unescapes on write.
      {[
        # L.view L.String.as_escaped "'\t' vs ' '"
        - : string = "'\\t' vs ' '"
      ]} *)

  val split_on_char : char -> (t, t list, t list, t) optic
  (** An isomorphism between separated string and list of strings.
      {[
        # L.over
            (L.String.split_on_char ':' % L.List.elems)
            String.capitalize_ascii "one:two:three"
        - : string = "One:Two:Three"
      ]} *)

  val starts_with : t -> (t, t, t, t) optic
  (** A prism that focuses on the suffix of a string that starts with the given
      prefix. *)

  val sub : int -> int -> (t, t, t, t) optic
  (** [sub pos len] is a lens that focuses on the substring that starts at
      specified [pos]ition and has given [len]gth.
      {[
        # L.set (L.String.sub 1 3) "ok" "Lasso"
        - : string = "Loko"
      ]} *)

  val trim : (t, t, t, t) optic
  (** An isomorphism that [trim]s the string in both directions. *)

  val as_unescaped : (t, t, t, t) optic
  (** An isomorphism that unescapes on read and escapes on write. *)
end
