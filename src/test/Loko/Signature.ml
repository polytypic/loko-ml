(* DO NOT EDIT! THIS IS AUTO EXTRACTED FROM THE SIGNATURE. *)

[@@@ocaml.warning "-8"] (* Disable non-exhaustive match warning. *)
[@@@ocaml.warning "-32"] (* Disable unused value warning. *)

module L = Loko

let ( ^<< ) f g x = f (g x)
let plus n = L.iso (( + ) n) (Fun.flip ( - ) n)
let 3 = L.view (plus 1) 2
let 1 = L.review (plus 2) 3
let 1 = L.view (L.re (plus 2)) 3

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
let 9 = L.transform (L.over_op (( + ) 2) |> L.and_then (L.over_op (( * ) 3))) 1

let "1", "2" =
  L.transform ((L.fst |> L.and_then L.snd) ^<< L.over_op Int.to_string) (1, 2)

let [3; 1; 4; 1] =
  L.remove (L.List.elems ^<< L.accept (fun x -> 4 < x)) [5; 3; 1; 6; 4; 1]

let [3; 1; 4; 1] =
  L.remove (L.List.elems ^<< L.reject (fun x -> x < 5)) [5; 3; 1; 6; 4; 1]

let 8 = L.fold 0 ( + ) L.List.elems [3; 1; 4]
let [|4; 2|] = L.view (L.Array.map (L.accept (( < ) 0))) [|4; -3; 2|]
let [|3; 1; 4|] = L.over (L.Array.at 2) (( + ) 1) [|3; 1; 3|]
let [|4; 2|] = L.remove (L.Array.at 2) [|4; 2; 1|]
let None = L.view_opt L.List.hd []
let 3 = L.view L.List.hd [3; 1; 4]
let [-3; 1; 4] = L.over L.List.hd ( ~- ) [3; 1; 4]
let [1; 4] = L.remove L.List.hd [3; 1; 4]
let None = L.view_opt L.List.tl []
let [1; 4] = L.view L.List.tl [3; 1; 4]
let [3; 4; 1] = L.over L.List.tl List.rev [3; 1; 4]
let [3] = L.remove L.List.tl [3; 1; 4]
let None = L.view L.List.to_option ["more"; "than"; "one"]
let (Some "exactly one") = L.view L.List.to_option ["exactly one"]
let None = L.view L.List.to_option []
let [3; 1; 4] = L.over L.List.rev (List.cons 4) [3; 1]

let [1; 2], [3; 4; 5] =
  L.view (L.List.partition (fun x -> x <= 2)) [3; 1; 4; 2; 5]

let [1; 2] = L.view (L.List.filter (fun x -> x <= 2)) [3; 1; 4; 2; 5]
let [3; 4; 5] = L.view (L.List.reject (fun x -> x <= 2)) [3; 1; 4; 2; 5]

module StringMap = struct
  include Map.Make (String)
  include L.Map.Make (Map.Make (String))
end

let a_string_map = L.review StringMap.to_assoc [("foo", 101); ("bar", 42)]
let 101 = L.view (StringMap.at "foo") a_string_map

let true =
  StringMap.equal ( = )
    (L.review StringMap.to_assoc [("bar", 42)])
    (L.remove (StringMap.at "foo") a_string_map)

let (Some 3) = L.over L.Option.elems (( + ) 2) (Some 1)
let None = L.remove L.Option.elems (Some 1)
let (Some 1) = L.review L.Option.elems 1
let "'\\t' vs ' '" = L.view L.String.escaped "'\t' vs ' '"

let "One:Two:Three" =
  L.over
    (L.String.split_on_char ':' ^<< L.List.elems)
    String.capitalize_ascii "one:two:three"

let "Loko" = L.set (L.String.sub 1 3) "ok" "Lasso"

(* DO NOT EDIT! THIS IS AUTO EXTRACTED FROM THE SIGNATURE. *)
