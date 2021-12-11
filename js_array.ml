open! Base
open! Js_of_ocaml

type 'a t = 'a Js.js_array Js.t

let empty () = new%js Js.array_empty
let clone (a : 'a t) = a##slice_end 0
let to_array = Js.to_array
let of_array = Js.array
let fast_to_array = to_array
let length (t : _ t) = t##.length
let unsafe_set_length (t : _ t) l = t##.length := l

let oob m t index =
  let length = length t in
  Base.Error.raise_s
    [%message m ": js array index out of bounds" (length : int) (index : int)]
;;

let bounds_check m t i = if i < 0 || i >= length t then oob m t i

let get_exn t i =
  bounds_check "get_exn" t i;
  Caml.Obj.magic (Js.array_get t i)
;;

let set_exn t i v =
  bounds_check "set_exn" t i;
  Js.array_set t i v
;;

let get_unchecked t i = Caml.Obj.magic (Js.array_get t i)
let set_unchecked t i v = Js.array_set t i v
let get t i = if i < 0 || i >= length t then None else Some (get_unchecked t i)

let equal eq a b =
  let la = length a in
  if phys_equal a b
  then true
  else if la <> length b
  then false
  else
    With_return.with_return (fun { return } ->
        for i = 0 to la - 1 do
          if not (eq (get_unchecked a i) (get_unchecked b i)) then return false
        done;
        return true)
;;

let compare cmp a b =
  let la = length a in
  let lb = length b in
  if phys_equal a b
  then 0
  else
    With_return.with_return (fun { return } ->
        for i = 0 to Int.min la lb - 1 do
          let c = cmp (get_unchecked a i) (get_unchecked b i) in
          if c <> 0 then return c
        done;
        return (lb - la))
;;

let push (t : _ t) v =
  let (_ : int) = t##push v in
  ()
;;

let unshift (t : _ t) v =
  let (_ : int) = t##unshift v in
  ()
;;

let pop (t : _ t) = if length t > 0 then Some (Caml.Obj.magic t##pop) else None
let shift (t : _ t) = if length t > 0 then Some (Caml.Obj.magic t##shift) else None

let pop_exn (t : _ t) =
  bounds_check "pop_exn" t 0;
  Caml.Obj.magic t##pop
;;

let shift_exn (t : _ t) =
  bounds_check "shift_exn" t 0;
  Caml.Obj.magic t##shift
;;

let concat (a : 'a t) (b : 'a t) = a##concat b

let singleton x =
  let a = empty () in
  push a x;
  a
;;

let sort (a : 'a t) ~(compare : 'a -> 'a -> int) =
  let cb : ('a -> 'a -> int) Js.callback = Js.wrap_callback compare in
  (* ints can be cast to floats without issue because they have the 
     same representation in javascript *)
  let cb : ('a -> 'a -> float) Js.callback = Caml.Obj.magic cb in
  let (_ : 'a t) = a##sort cb in
  ()
;;

let sorted a ~compare =
  let a = clone a in
  sort a ~compare;
  a
;;

let slice ?end_ ~start (a : _ t) =
  match end_ with
  | None -> a##slice_end start
  | Some end_ -> a##slice start end_
;;

let remove (t : _ t) idx =
  bounds_check "remove" t idx;
  let (_ : 'a t) = t##splice idx 1 in
  ()
;;

let swap_remove (t : _ t) idx =
  bounds_check "swap_remove" t idx;
  if idx = length t - 1 then pop_exn t else set_exn t idx (pop_exn t)
;;

let remove_range (t : _ t) idx ~len =
  bounds_check "remove" t idx;
  bounds_check "remove" t (idx + len);
  let (_ : 'a t) = t##splice idx len in
  ()
;;

let rev (t : _ t) =
  let (_ : 'a t) = t##reverse in
  ()
;;

let map (t : 'a t) ~(f : 'a -> 'b) = Js.array_map f t
let mapi (t : 'a t) ~(f : int -> 'a -> 'b) = Js.array_mapi f t

let filter (t : 'a t) ~f =
  t##filter (Js.wrap_callback (fun elt _idx _arr -> Js.bool (f elt)))
;;

let filteri (t : 'a t) ~f =
  t##filter (Js.wrap_callback (fun elt idx _arr -> Js.bool (f idx elt)))
;;

let insert (t : 'a t) ~idx a =
  if idx = 0
  then (* 0 is always ok *)
    ()
  else if idx = length t
  then (* using [insert] like "push" is also ok *)
    ()
  else bounds_check "insert" t idx;
  let (_ : _ t) = t##splice_1 idx 1 a in
  ()
;;

(*_ let copy_within (t : _ t) ~target ~start ~end_ = 
    t##copyWithin target start end_ *)

let rev_iter (t : _ t) ~f =
  for i = length t - 1 downto 0 do
    f (get_exn t i)
  done
;;

let rev_iteri (t : _ t) ~f =
  for i = length t - 1 downto 0 do
    f i (get_exn t i)
  done
;;

let sexp_of_t sexp_of_a (t : _ t) =
  let l = ref [] in
  rev_iter t ~f:(fun a -> l := sexp_of_a a :: !l);
  Sexp.List !l
;;

let t_of_sexp a_of_sexp sexp =
  let a = empty () in
  (match sexp with
  | Sexp.Atom _ -> failwith "expected list, found atom"
  | List l ->
    unsafe_set_length a (List.length l);
    List.iteri l ~f:(fun i s -> set_unchecked a i (a_of_sexp s)));
  a
;;

include Bin_prot.Utils.Make_binable1_with_uuid (struct
  module Binable = struct
    open Bin_prot.Std

    type 'a t = 'a array [@@deriving bin_io]
  end

  type nonrec 'a t = 'a t

  let of_binable = of_array
  let to_binable = to_array

  let caller_identity =
    Bin_shape_lib.Std.Shape.Uuid.of_string "F8686F64-7BCC-4F34-B09B-CCD888149264"
  ;;
end)

include Indexed_container.Make (struct
  type nonrec 'a t = 'a t

  let iter (t : _ t) ~f =
    for i = 0 to length t - 1 do
      f (get_exn t i)
    done
  ;;

  let iteri (t : _ t) ~f =
    for i = 0 to length t - 1 do
      f i (get_exn t i)
    done
  ;;

  let fold (t : _ t) ~init ~f =
    let acc = ref init in
    for i = 0 to length t - 1 do
      acc := f !acc (get_exn t i)
    done;
    !acc
  ;;

  let foldi (t : _ t) ~init ~f =
    let acc = ref init in
    for i = 0 to length t - 1 do
      acc := f i !acc (get_exn t i)
    done;
    !acc
  ;;

  let fold = fold
  let iter = `Custom iter
  let iteri = `Custom iteri
  let foldi = `Custom foldi
  let length = `Custom length
end)

include Binary_searchable.Make1 (struct
  type nonrec 'a t = 'a t

  let length = length
  let get = get_exn
end)

include Blit.Make1 (struct
  type nonrec 'a t = 'a t

  let length = length

  let create_like ~len t =
    let my_length = length t in
    let clone = concat t (empty ()) in
    if len = my_length
    then ()
    else if len < my_length
    then unsafe_set_length clone len
    else (
      let last = get_exn clone 0 in
      for _ = 0 to my_length - len - 1 do
        push clone last
      done);
    assert false
  ;;

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    for i = 0 to len - 1 do
      set_unchecked dst (dst_pos + i) (get_unchecked src (src_pos + i))
    done
  ;;
end)

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let map = `Custom map
  let return = singleton

  let bind t ~f =
    let a = empty () in
    iter t ~f:(fun x -> iter (f x) ~f:(fun y -> push a y));
    a
  ;;
end)

include Comparator.Derived (struct
  type nonrec 'a t = 'a t [@@deriving sexp, compare]
end)

let to_array = fast_to_array
