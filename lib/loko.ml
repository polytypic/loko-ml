let uncurry f (x, y) = f x y

module Infix = struct
  let[@inline] ( % ) f g x = f (g x)
end

open Infix

(* *)

module Iterator = struct
  type ('xs, 'x) t =
    | T : { start : 'xs -> 'iterator; next : 'iterator -> 'x } -> ('xs, 'x) t

  exception Done

  let array =
    let open struct
      type 'x t = { mutable i : int; xs : 'x Array.t }
    end in
    let start xs = { i = 0; xs }
    and next t =
      let i = t.i in
      if i < Array.length t.xs then begin
        t.i <- i + 1;
        Array.unsafe_get t.xs i
      end
      else raise_notrace Done
    in
    T { start; next }

  let list =
    let start = ref
    and next t =
      match !t with
      | [] -> raise_notrace Done
      | x :: xs ->
          t := xs;
          x
    in
    T { start; next }
end

module Builder = struct
  type ('y, 'ys) t =
    | T : {
        empty : unit -> 'builder;
        add : 'builder -> 'y -> unit;
        finish : 'builder -> 'ys;
      }
        -> ('y, 'ys) t

  let array =
    let empty = Dynarray.create in
    let add = Dynarray.add_last in
    let finish = Dynarray.to_array in
    T { empty; add; finish }

  let list =
    let empty = Dynarray.create in
    let add = Dynarray.add_last in
    let finish = Dynarray.to_list in
    T { empty; add; finish }
end

(* ************************************************************************** *)

module Magic : sig
  type +'a t

  val mock : 'a t
  val inj : 'a -> 'a t
  val unsafe_prj : 'a t -> 'a
end = struct
  type +'a t = 'a option

  let mock = None
  let inj = Option.some
  let unsafe_prj = Option.get
end

(* *)

type context = {
  over : bool;
  mutable hit : bool;
  mutable index : int;
  mutable view : exn;
}

type dir = Fwd | Bwd
type (-'S, +'T) d = context -> 'S Magic.t -> 'T Magic.t
type (-'S, +'T) pipe = dir * ('S, 'T) d
type (-'S, +'F, -'G, +'T) t = ('F, 'G) pipe -> ('S, 'T) pipe

(* *)

exception Miss
exception CanView

(* *)

let memo o p =
  let state = ref @@ Either.Left o in
  fun c s ->
    let fn =
      match !state with
      | Left o ->
          let _, fn = o p in
          state := Right fn;
          fn
      | Right fn -> fn
    in
    fn c s

(* *)

let clear_hit c =
  let h = c.hit in
  if h then c.hit <- false;
  h

let set_hit c =
  c.hit <- true;
  Magic.mock

let set_hit_on_over c =
  if c.over then c.hit <- true;
  Magic.mock

(* *)

let iso fwd bwd =
 fun (dir, p) ->
  ( dir,
    fun c s ->
      let g =
        p c
          (if dir = Bwd then Magic.mock
           else Magic.inj (fwd (Magic.unsafe_prj s)))
      in
      if c.hit || not c.over then Magic.mock
      else Magic.inj (bwd (Magic.unsafe_prj g)) )

let lens get set =
 fun (_, p) ->
  ( Fwd,
    fun c s ->
      let s = Magic.unsafe_prj s in
      let g = p c (Magic.inj (get s)) in
      if c.hit || not c.over then Magic.mock
      else Magic.inj (set (Magic.unsafe_prj g) s) )

let partial_lens get_opt set remove =
 fun (_, p) ->
  ( Fwd,
    fun c s ->
      let s = Magic.unsafe_prj s in
      match get_opt s with
      | None -> if c.over then Magic.inj (remove s) else Magic.mock
      | Some f ->
          let g = p c (Magic.inj f) in
          if c.over then
            Magic.inj
              (if clear_hit c then remove s else set (Magic.unsafe_prj g) s)
          else Magic.mock )

let getter get =
 fun (_, p) ->
  ( Fwd,
    fun c s ->
      let f = Magic.inj (get (Magic.unsafe_prj s)) in
      p c f |> ignore;
      s )

let prism sft gt : _ t =
 fun (dir, p) ->
  ( dir,
    fun c s ->
      let bwd f =
        let g = p c f in
        if c.hit || not c.over then Magic.mock
        else Magic.inj (gt (Magic.unsafe_prj g))
      in
      if dir = Bwd then bwd Magic.mock
      else
        match (sft (Magic.unsafe_prj s) : _ Either.t) with
        | Left t -> if c.over then Magic.inj t else Magic.mock
        | Right f -> bwd (Magic.inj f) )

let branch'2 o1 o2 =
 fun p ->
  let p1 = memo o1 p and p2 = memo o2 p in
  ( Fwd,
    fun c s ->
      let x1, x2 = Magic.unsafe_prj s in
      let x1 = p1 c (Magic.inj x1) in
      if c.hit then Magic.mock
      else
        let x2 = p2 c (Magic.inj x2) in
        if c.hit || not c.over then Magic.mock
        else Magic.inj (Magic.unsafe_prj x1, Magic.unsafe_prj x2) )

let traversal (Iterator.T { start; next }) (Builder.T { empty; add; finish }) =
 fun (_, p) ->
  ( Fwd,
    fun c xs ->
      let xs = Magic.unsafe_prj xs in
      if c.over then begin
        let br = empty () in
        let ir = start xs in
        begin
          try
            while true do
              let x = next ir in
              let y = p c (Magic.inj x) in
              if not (clear_hit c) then add br (Magic.unsafe_prj y)
            done
          with Iterator.Done -> ()
        end;
        Magic.inj (finish br)
      end
      else begin
        begin
          let ir = start xs in
          try
            while not c.hit do
              let x = next ir in
              p c (Magic.inj x) |> ignore
            done
          with Iterator.Done -> ()
        end;
        Magic.mock
      end )

(* *)

let zero _ = (Fwd, fun _ s -> s)

(* *)

let if_else predicate on_true on_false p =
  let p_t = memo on_true p and p_f = memo on_false p in
  (Fwd, fun c s -> if predicate (Magic.unsafe_prj s) then p_t c s else p_f c s)

let choose to_optic p =
  ( Fwd,
    fun c s ->
      let _, p = to_optic (Magic.unsafe_prj s) p in
      p c s )

(* *)

let removed_as value (dir, p) =
  ( dir,
    fun c x ->
      let y = p c x in
      if c.over then if clear_hit c then Magic.inj value else y else Magic.mock
  )

let removed_as_none (dir, p) =
  ( dir,
    fun c x ->
      let y = p c x in
      if c.over then
        if clear_hit c then Magic.inj None
        else Magic.inj @@ Some (Magic.unsafe_prj y)
      else Magic.mock )

let none_as_removed (dir, p) =
  ( dir,
    fun c x ->
      let yO = p c x in
      if c.hit || not c.over then Magic.mock
      else
        match Magic.unsafe_prj yO with
        | None -> set_hit c
        | Some y -> Magic.inj y )

let removed_if predicate (dir, p) =
  ( dir,
    fun c x ->
      let y = p c x in
      if c.hit || not c.over then Magic.mock
      else if predicate (Magic.unsafe_prj y) then set_hit c
      else y )

(* *)

let before action (dir, p) =
  ( dir,
    fun c s ->
      if not c.over then action (Magic.unsafe_prj s);
      p c s )

let after ?(on_removed = ignore) on_updated (dir, p) =
  ( dir,
    fun c s ->
      let t = p c s in
      if c.over then
        if c.hit then on_removed () else on_updated (Magic.unsafe_prj t);
      t )

(* *)

let get_at i xs =
  let i = !i in
  if i < 0 || Array.length xs <= i then None else Some (Array.unsafe_get xs i)

let remove_at i xs =
  let i = !i in
  let n = Array.length xs in
  if i < 0 || n <= i then xs
  else
    let ys = Array.make (n - 1) xs.(0) in
    Array.blit xs 0 ys 0 i;
    Array.blit xs (i + 1) ys i (n - 1 - i);
    ys

let set_at i y xs =
  let ys = Array.copy xs in
  ys.(!i) <- y;
  ys

let at_ref i = partial_lens (get_at i) (set_at i) (remove_at i)
let at i = at_ref (ref i)

(* *)

let disperse_keep o values =
  let _, p =
    o
      ( Fwd,
        fun c x ->
          let i = c.index in
          if i < Array.length values then begin
            c.index <- i + 1;
            Magic.inj (Array.unsafe_get values i)
          end
          else x )
  in
  fun s ->
    let c = { over = true; hit = false; index = 0; view = Miss } in
    let r = p c (Magic.inj s) in
    if c.hit then failwith "disperse_keep";
    Magic.unsafe_prj r

let disperse o values =
  let _, p =
    o
      ( Fwd,
        fun c _ ->
          let i = c.index in
          if i < Array.length values then begin
            c.index <- i + 1;
            Magic.inj (Array.unsafe_get values i)
          end
          else set_hit c )
  in
  fun s ->
    let c = { over = true; hit = false; index = 0; view = Miss } in
    let r = p c (Magic.inj s) in
    if c.hit then failwith "disperse";
    Magic.unsafe_prj r

(* *)

let remove_op _ = (Fwd, fun c _ -> set_hit_on_over c)
let over_op fn _ = (Fwd, fun _ s -> Magic.inj (fn (Magic.unsafe_prj s)))

(* *)

let and_then o2 o1 p =
  let p1 = memo o1 p in
  let p2 = memo o2 p in
  ( Fwd,
    fun c s0 ->
      let s1 = p1 c s0 in
      if c.hit then Magic.mock else p2 c s1 )

(* *)

let view_with optic view finish =
  let _, p =
    optic
      ( Fwd,
        fun c x ->
          c.view <- view (Magic.unsafe_prj x);
          set_hit c )
  in
  fun s ->
    let c = { over = false; hit = false; index = 0; view = Miss } in
    p c (Magic.inj s) |> ignore;
    finish c.view

let view (type f) o =
  let exception Hit of f in
  view_with o (fun x -> Hit x) (function Hit x -> x | _ -> failwith "view")

let can_view o =
  view_with o (fun _ -> CanView) (function CanView -> true | _ -> false)

let view_opt (type f) o =
  let exception Hit of f in
  view_with o (fun x -> Hit x) (function Hit x -> Some x | _ -> None)

(* *)

let review_with (type g) optic finish =
  let exception Hit of g in
  let dir, p =
    optic
      (Bwd, fun c _ -> match c.view with Hit g -> Magic.inj g | e -> raise e)
  in
  if dir <> Bwd then failwith "review";
  fun f ->
    let c = { over = true; hit = false; index = 0; view = Hit f } in
    let t = p c Magic.mock in
    finish c.hit t

let can_review optic = review_with optic @@ fun h _ -> not h

let review optic =
  review_with optic @@ fun h t ->
  if h then failwith "review" else Magic.unsafe_prj t

let review_opt optic =
  review_with optic @@ fun h t -> if h then None else Some (Magic.unsafe_prj t)

(* *)

let over_with optic mapping finish =
  let _, p = optic (Fwd, fun _ x -> Magic.inj (mapping (Magic.unsafe_prj x))) in
  fun s ->
    let c = { over = true; hit = false; index = 0; view = Miss } in
    let r = p c (Magic.inj s) in
    finish c.hit r s

let can_over o fn = over_with o fn @@ fun h _ _ -> not h

let over o fn =
  over_with o fn @@ fun h r _ ->
  if h then failwith "over" else Magic.unsafe_prj r

let over_default o fn =
  over_with o fn @@ fun h r s -> if h then s else Magic.unsafe_prj r

let over_opt o fn =
  over_with o fn @@ fun h r _ -> if h then None else Some (Magic.unsafe_prj r)

(* *)

let iter o action =
  let _, p =
    o
      ( Fwd,
        fun _ x ->
          action (Magic.unsafe_prj x);
          Magic.mock )
  in
  fun s ->
    let c = { over = false; hit = false; index = 0; view = Miss } in
    p c (Magic.inj s) |> ignore

(* ************************************************************************** *)

let can_set o value = can_over o (Fun.const value)
let set o value = over o (Fun.const value)
let set_default o value = over_default o (Fun.const value)
let set_opt o value = over_opt o (Fun.const value)

(* *)

let can_transform o = can_over o Fun.id
let transform o = over o Fun.id
let transform_opt o = over_opt o Fun.id
let transform_default o = over_default o Fun.id

(* *)

let can_remove o = can_transform (o % remove_op)
let remove o = transform (o % remove_op)
let remove_opt o = transform_opt (o % remove_op)
let remove_default o = transform_default (o % remove_op)

(* *)

let set_op value = over_op (Fun.const value)

(* *)

let or_else o2 o1 = if_else (can_view o1) o1 o2

let tuple'2 o1 o2 =
  lens
    (fun s -> (view o1 s, view o2 s) (* TODO *))
    (fun (v1, v2) s -> set o2 v2 (set o1 v1 s))

(* *)

let branch'3 o1 o2 o3 =
  iso (fun (x1, x2, x3) -> ((x1, x2), x3)) (fun ((x1, x2), x3) -> (x1, x2, x3))
  % branch'2 (branch'2 o1 o2) o3

let branch'4 o1 o2 o3 o4 =
  iso
    (fun (x1, x2, x3, x4) -> ((x1, x2), (x3, x4)))
    (fun ((x1, x2), (x3, x4)) -> (x1, x2, x3, x4))
  % branch'2 (branch'2 o1 o2) (branch'2 o3 o4)

(* *)

let accept pr = prism (fun x -> if pr x then Right x else Left x) Fun.id
let reject pr = prism (fun x -> if pr x then Left x else Right x) Fun.id

(* *)

let fold zero plus o s =
  let r = ref zero in
  iter o (fun x -> r := plus !r x) s;
  !r

let count o = fold 0 (fun n _ -> n + 1) o

let collect_with (Builder.T { empty; add; finish }) o s =
  let br = empty () in
  iter o (add br) s;
  finish br

let collect o = collect_with Builder.list o
let exists predicate o = can_view (o % accept predicate)
let forall predicate o = not % can_view (o % reject predicate)
let disjunction o = exists Fun.id o
let conjunction o = forall Fun.id o
let concat sep o = String.concat sep % collect o

(* *)

let cond_of o to_els = to_els o
let case whn thn to_els o = if_else (exists whn o) thn (to_els o)
let otherwise els _ = els

(* *)

let parts_of o = lens (collect_with Builder.array o) (disperse o)
let parts_of_keep o = lens (collect_with Builder.array o) (disperse_keep o)

(* *)

let fold_lens fold traversal = lens (fold traversal) (set traversal)

(* *)

let re invertible = iso (review invertible) (view invertible)
let id p = iso Fun.id Fun.id p
let reread fn = iso fn Fun.id
let rewrite fn = iso Fun.id fn
let normalize fn = iso fn fn
let subset pred = reread (fun inn -> if pred inn then Some inn else None)

(* *)

let apply_at tv tr o =
  iso
    (over (tv % none_as_removed) (view_opt o))
    (over (tr % none_as_removed) (review_opt o))

let iterate o =
  let rec iterate fn x = match fn x with None -> x | Some x -> iterate fn x in
  iso (iterate (view_opt o)) (iterate (review_opt o))

(* *)

let as_removed value = removed_if (( = ) value)
let removed_option p = if_else Option.is_none remove_op (reread Option.get) p

(* *)

let to_default' value = Option.value ~default:value
let of_default' value v = if v = value then None else Some v
let to_default value = reread (to_default' value)
let of_default value = rewrite (of_default' value)
let default value = iso (to_default' value) (of_default' value)

let is_or ~falsy ~truthy =
  iso (( = ) truthy) (function false -> falsy | true -> truthy)

(* *)

let fst p = lens fst (fun x (_, y) -> (x, y)) p
let snd p = lens snd (fun y (x, _) -> (x, y)) p

let iso_pair i1 i2 =
  let cross f1 f2 (v1, v2) = (f1 v1, f2 v2) in
  iso (cross (view i1) (view i2)) (cross (review i1) (review i2))

(* *)

let elem_1_of_2 = fst
let elem_2_of_2 = snd

let elem_1_of_3 p =
  lens (fun (x1, _, _) -> x1) (fun x1 (_, x2, x3) -> (x1, x2, x3)) p

let elem_2_of_3 p =
  lens (fun (_, x2, _) -> x2) (fun x2 (x1, _, x3) -> (x1, x2, x3)) p

let elem_3_of_3 p =
  lens (fun (_, _, x3) -> x3) (fun x3 (x1, x2, _) -> (x1, x2, x3)) p

let elem_1_of_4 p =
  lens (fun (x1, _, _, _) -> x1) (fun x1 (_, x2, x3, x4) -> (x1, x2, x3, x4)) p

let elem_2_of_4 p =
  lens (fun (_, x2, _, _) -> x2) (fun x2 (x1, _, x3, x4) -> (x1, x2, x3, x4)) p

let elem_3_of_4 p =
  lens (fun (_, _, x3, _) -> x3) (fun x3 (x1, x2, _, x4) -> (x1, x2, x3, x4)) p

let elem_4_of_4 p =
  lens (fun (_, _, _, x4) -> x4) (fun x4 (x1, x2, x3, _) -> (x1, x2, x3, x4)) p

(* *)

let truncate = iso truncate float_of_int

(* *)

module Array = struct
  include Array

  let as_list p = iso Array.to_list Array.of_list p
  let iterator = Iterator.array
  let builder = Builder.array
  let elems p = traversal iterator builder p
  let map i = apply_at elems elems i

  let as_option p =
    iso
      (function [| x |] -> Some x | _ -> None)
      (function None -> [||] | Some x -> [| x |])
      p

  let at = at
  let at_ref = at_ref
end

module List = struct
  include List

  let iterator = Iterator.list
  let builder = Builder.list
  let decon = function [] -> Either.Left [] | x :: xs -> Right (x, xs)

  let hd p =
    prism decon (function None, xs -> xs | Some x, xs -> x :: xs)
    @@ fst @@ removed_as_none @@ p

  let tl p =
    prism decon (function x, None -> [ x ] | x, Some xs -> x :: xs)
    @@ snd @@ removed_as_none @@ p

  let as_array p = iso Array.of_list Array.to_list p
  let elems p = traversal iterator builder p
  let map i = apply_at elems elems i

  let as_option p =
    iso
      (function [ x ] -> Some x | _ -> None)
      (function None -> [] | Some x -> [ x ])
      p

  let as_rev p = iso List.rev List.rev p
  let partition predicate = iso (List.partition predicate) (uncurry List.append)
  let filter predicate = partition predicate % fst
  let reject predicate = partition predicate % snd
end

module Either = struct
  open Either

  let map left right =
    iso
      (map ~left:(view left) ~right:(view right))
      (map ~left:(review left) ~right:(review right))

  let on_left p =
    prism (function Left x -> Right x | Right x -> Left (Right x)) left p

  let on_right p =
    prism (function Left x -> Left (Left x) | Right x -> Right x) right p
end

module Option = struct
  open Option

  let on_some p =
    prism (function Some value -> Right value | None -> Left None) some p

  let elems p = removed_as None @@ on_some @@ p
  let map i = apply_at elems elems i
end

module Map = struct
  module Make (M : Map.S) = struct
    let at k =
      lens (M.find_opt k) (function None -> M.remove k | Some v -> M.add k v)
      % Option.elems

    let as_assoc p = iso M.bindings (M.of_seq % List.to_seq) p
    let elems p = as_assoc @@ List.elems @@ snd @@ p
  end
end

module Result = struct
  open Result

  let map on_ok on_error = function
    | Ok o -> Ok (on_ok o)
    | Error e -> Error (on_error e)

  let map ok error =
    iso (map (view ok) (view error)) (map (review ok) (review error))

  let on_ok p =
    prism (function Ok x -> Right x | Error x -> Left (Error x)) ok p

  let on_error p =
    prism (function Ok x -> Left (Ok x) | Error x -> Right x) error p
end

module String = struct
  open String

  let ends_with suffix =
    prism
      (fun string ->
        if ends_with ~suffix string then
          let n = length suffix in
          Right (sub string 0 (length string - n))
        else Left string)
      (fun prefix -> prefix ^ suffix)

  let as_escaped p = iso String.escaped Scanf.unescaped p

  let starts_with prefix =
    prism
      (fun string ->
        if starts_with ~prefix string then
          let n = length prefix in
          Right (sub string n (length string - n))
        else Left string)
      (fun suffix -> prefix ^ suffix)

  let split_on_char sep = iso (split_on_char sep) (concat (make 1 sep))

  let sub pos len =
    lens
      (fun s -> sub s pos len)
      (fun x s ->
        let aft = pos + len in
        sub s 0 pos ^ x ^ sub s aft (length s - aft))

  let trim p = normalize trim p
  let as_unescaped p = iso Scanf.unescaped String.escaped p
end
