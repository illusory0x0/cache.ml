let panic () = failwith "panic"

module List = struct
  type (_, _) t = Nil : ('a, 'b) t
end

module Cache = struct
  let do_nothing () = ()

  type 'a list = [] : 'a t list | ( :: ) : 'a t * 'b t list -> 'c t list

  and 'a t = {
    mutable cache : 'a;
    init : unit -> 'a;
    mutable children : 'a t list;
    mutable refresh : unit -> unit;
  }

  let pp (ppv : Format.formatter -> 'a -> unit) (ppf : Format.formatter)
      (self : 'a t) =
    ppv ppf self.cache

  type 'a children = 'a t list

  let rec refresh_children : type a. a list -> unit = function
    | [] -> ()
    | x :: xs ->
        x.refresh ();
        refresh_children xs

  let refresh self = refresh_children self.children

  let from_fun (init : unit -> 'a) : 'a t =
    let self = { cache = init (); init; children = []; refresh = do_nothing } in
    let _ =
      self.refresh <-
        (fun () ->
          self.cache <- self.init ();
          refresh self)
    in
    self

  let from_val value = from_fun (fun () -> value)
  let require_by self other = self.children <- other :: self.children
  let depend_on self other = require_by other self
end

module State = struct
  type 'a t = { mutable value : 'a; mutable children : 'a Cache.children }

  let pp (ppv : Format.formatter -> 'a -> unit) (ppf : Format.formatter)
      (self : 'a t) =
    ppv ppf self.value

  let get self = self.value

  let set self value =
    self.value <- value;
    Cache.refresh_children self.children

  let require_by self other = self.children <- other :: self.children
  let modify self f = set self (f self.value)
  let depend_on self other = require_by other self
  let make value = { value; children = Cache.[] }
end

module Test = struct
  type t = Lit of int State.t | Add of int Cache.t
  [@@deriving show { with_path = false }]

  let lit v = Lit v

  let add x y =
    match (x, y) with
    | Lit x, Lit y ->
        let cache =
          Cache.from_fun (fun _ ->
              print_endline "case 1";
              x.value + y.value)
        in
        State.depend_on cache x;
        State.depend_on cache y;
        Add cache
    | Lit x, Add y ->
        let cache =
          Cache.from_fun (fun _ ->
              print_endline "case 2";
              x.value + y.cache)
        in
        State.depend_on cache x;
        Cache.depend_on cache y;
        Add cache
    | Add x, Lit y ->
        let cache =
          Cache.from_fun (fun _ ->
              print_endline "case 3";
              x.cache + y.value)
        in
        Cache.depend_on cache x;
        State.depend_on cache y;
        Add cache
    | Add x, Add y ->
        let cache =
          Cache.from_fun (fun _ ->
              print_endline "case 4";
              x.cache + y.cache)
        in
        Cache.depend_on cache x;
        Cache.depend_on cache y;
        Add cache

  let ( + ) = add

  let ex =
    let x = State.make 3 in
    let y = State.make 5 in
    let z = State.make 8 in
    let w = State.make 75 in

    let expr = Lit x + Lit y + (Lit z + Lit w) in
    let ppln expr =
      pp Format.std_formatter expr;
      Format.pp_print_newline Format.std_formatter ()
    in

    ppln expr;
    State.modify x (fun x -> x * x * x);
    ppln expr;
    (* print_int expr.  *)
    ()
end
