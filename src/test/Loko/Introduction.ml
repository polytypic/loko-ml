(* DO NOT EDIT! THIS IS AUTO EXTRACTED FROM THE README. *)

[@@@ocaml.warning "-8"] (* Disable non-exhaustive match warning. *)

module L = Loko

let 42 = L.view L.fst (42, "answer")
let "answer" = L.view L.snd (42, "answer")
let "42", "answer" = L.over L.fst Int.to_string (42, "answer")
let ( ^<< ) f g x = f (g x)
let "1" = L.view (L.fst ^<< L.snd) ((3, "1"), ('4', true))

let (3, 1), ('4', true) =
  L.over (L.fst ^<< L.snd) int_of_string ((3, "1"), ('4', true))

let [3; 1; 4] = L.over L.List.elems (( + ) 1) [2; 0; 3]

let [(4, 2); (7, 6)] =
  L.over (L.List.elems ^<< L.fst) (( + ) 1) [(3, 2); (6, 6)]

let 4 = L.view L.List.elems [4; 1]
let None = L.view_opt L.List.elems []
let (Some 1) = L.view_opt L.List.elems [1; 1; 2]
let 9 = L.fold 0 ( + ) (L.List.elems ^<< L.fst) [(3, 2); (6, 6)]
let [3; 6] = L.collect (L.List.elems ^<< L.fst) [(3, 2); (6, 6)]
let [|3; 6|] = L.view (L.parts_of (L.List.elems ^<< L.fst)) [(3, 2); (6, 6)]

let [(6, 2); (3, 6)] =
  L.over
    (L.parts_of (L.List.elems ^<< L.fst) ^<< L.Array.to_list)
    List.rev
    [(3, 2); (6, 6)]

let plus n = L.iso (fun x -> x + n) (fun x -> x - n)
let 3 = L.view (plus 2) 1
let 1 = L.set (plus 2) 3 42
let 1 = L.view (L.re (plus 2)) 3
let 3 = L.set (L.re (plus 2)) 1 42
let 1 = L.review (plus 2) 3
let 2305843009213693952 = 1 lsl 61
let 2.30584300921369395e+18 = L.review L.truncate (1 lsl 61)
let (Some 1) = L.review L.Option.elems 1

let [3; -1; 1; -2; -3; -4; 4] =
  L.collect (L.Array.elems ^<< L.List.elems) [|[3; -1; 1]; [-2; -3]; [-4; 4]|]

let [-1; -2; -3; -4] =
  L.collect
    (L.Array.elems ^<< L.List.elems ^<< L.accept (fun x -> x < 0))
    [|[3; -1; 1]; [-2; -3]; [-4; 4]|]

let [|[3; 1]; []; [4]|] =
  L.remove
    (L.Array.elems ^<< L.List.elems ^<< L.accept (fun x -> x < 0))
    [|[3; -1; 1]; [-2; -3]; [-4; 4]|]

let [|[3; 1]; [4]|] =
  L.remove
    (L.Array.elems ^<< L.as_removed [] ^<< L.List.elems
    ^<< L.accept (fun x -> x < 0))
    [|[3; -1; 1]; [-2; -3]; [-4; 4]|]

let false = L.can_remove L.fst ("Computer", "says no")

let true =
  L.can_remove (L.Option.elems ^<< L.fst) (Some ("Computer", "says no"))

let None = L.remove (L.Option.elems ^<< L.fst) (Some ("Computer", "says no"))

let None, "says no" =
  L.remove (L.fst ^<< L.Option.elems) (Some "Computer", "says no")

let None, "says no" =
  L.remove (L.fst ^<< L.removed_as_none) ("Computer", "says no")

let "Nobody", "says no" =
  L.remove (L.fst ^<< L.removed_as "Nobody") ("Computer", "says no")

let eta'1 fn x1 x2 = fn x1 x2

type ('k, 'v) bt = [`Lf | `Br of ('k, 'v) bt * 'k * 'v * ('k, 'v) bt]

let a_tree : (_, _) bt =
  `Br
    ( `Br (`Lf, 2, "s", `Br (`Lf, 4, "o", `Lf)),
      5,
      "g",
      `Br (`Lf, 7, "i", `Br (`Lf, 11, "c", `Lf)) )

let on_br p =
  p |> L.prism (function `Br x -> `Hit x | `Lf -> `Miss `Lf) (fun x -> `Br x)

let key p = p |> on_br ^<< L.elem_2_of_4
let smaller p = p |> on_br ^<< L.elem_1_of_4
let greater p = p |> on_br ^<< L.elem_4_of_4

let rec naive_bst p =
  L.rewrite (function
    | `Lf -> `Lf
    | `Br (l, k, Some v, r) -> `Br (l, k, v, r)
    | `Br (`Lf, _, None, t) | `Br (t, _, None, `Lf) -> t
    | `Br (`Br (l, k, v, m), _, None, r) ->
      L.set (node_of k) (`Br (l, k, Some v, m)) r)
  @@ L.removed_as `Lf @@ p

and node_of k' =
  L.cond_of key
  @@ L.case (fun k -> k' < k) (smaller ^<< eta'1 node_of k')
  @@ L.case (fun k -> k < k') (greater ^<< eta'1 node_of k')
  @@ L.otherwise naive_bst

let value_of k =
  node_of k
  ^<< L.lens
        (function `Lf -> None | `Br (_, _, v, _) -> Some v)
        (fun v -> function
          | (`Lf as l as r) | `Br (l, _, _, r) -> `Br (l, k, v, r))
  ^<< L.removed_as_none

let rec inorder p =
  p
  |> naive_bst ^<< on_br ^<< L.branch'4 inorder L.zero L.removed_as_none inorder

let "M-a-g-i-c" =
  a_tree
  (* Update: *)
  |> L.set (value_of 2) "M"
  (* Delete: *)
  |> L.remove (value_of 4)
  (* Create: *)
  |> L.set (value_of 3) "a"
  (* Read: *)
  |> L.concat "-" inorder

(* DO NOT EDIT! THIS IS AUTO EXTRACTED FROM THE README. *)
