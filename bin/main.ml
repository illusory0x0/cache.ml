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
    mutable children : 'a t list; (* if A depend on B then A is B of child *)
    mutable co_children : 'a t list;
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

  let rec height : type a. a t -> int =
   fun self ->
    let rec go : type a. a t list -> int -> int =
     fun xs acc ->
      match xs with [] -> acc | x :: xs -> go xs (Int.max (height x + 1) acc)
    in
    go self.co_children 1
  (* 'a State.t is NIL, neither Internal Node nor Leaf Node *)

  let refresh self = refresh_children self.children

  let from_fun (init : unit -> 'a) : 'a t =
    let self =
      {
        cache = init ();
        init;
        children = [];
        refresh = do_nothing;
        co_children = [];
      }
    in
    let _ =
      self.refresh <-
        (fun () ->
          self.cache <- self.init ();
          refresh self)
    in
    self

  let from_val value = from_fun (fun () -> value)

  let require_by self other =
    self.children <- other :: self.children;
    other.co_children <- self :: other.children

  let depend_on self other = require_by other self
end

module State = struct
  type 'a t = { 
    mutable value : 'a;
     mutable children : 'a Cache.children;
     mutable co_children : 'a Cache.children 
    }

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
  let make value = { value; children = []; co_children = [] }
end

module Test = struct
  type t = Lit of int State.t | Add of int Cache.t
  [@@deriving show { with_path = false }]

  let fromAdd = function Add x -> x | _ -> panic ()
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
    print_endline "basic implentation";
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
    print_string "height: ";
    print_int (fromAdd expr |> Cache.height);
    print_newline ();

    print_newline ();
    ()
end

module CacheOO = struct
  class virtual ['a] required_by =
    object
      method virtual required_by : 'a Cache.t -> unit
    end

  class ['a] cache (f : unit -> 'a) =
    object
      inherit ['a] required_by
      val repr = Cache.from_fun f
      method required_by other = Cache.require_by repr other
      method cache = repr.cache
      method unwrap = repr
      method height = Cache.height repr
    end

  class ['a] state (v : 'a) =
    object
      inherit ['a] required_by
      val repr = State.make v
      method required_by other = State.require_by repr other
      method value = repr.value
      method unwrap = repr
      method modify = State.modify repr
    end

  let pp_state (ppv : Format.formatter -> 'a -> unit) (ppf : Format.formatter)
      (self : 'a state) =
    State.pp ppv ppf self#unwrap

  let pp_cache (ppv : Format.formatter -> 'a -> unit) (ppf : Format.formatter)
      (self : 'a cache) =
    Cache.pp ppv ppf self#unwrap

  let depend_on (self : 'a cache)
      (other : < required_by : 'a Cache.t -> 'b ; .. >) : unit =
    other#required_by self#unwrap

  let ( >> ) = depend_on
end

module Test_OO = struct
  open CacheOO

  type t = Lit of int state | Add of int cache
  [@@deriving show { with_path = false }]

  let lit v = Lit v
  let fromAdd = function Add x -> x | _ -> panic ()

  let add x y =
    match (x, y) with
    | Lit x, Lit y ->
        let cache =
          new cache (fun _ ->
              print_endline "case 1";
              x#value + y#value)
        in
        cache >> x;
        cache >> y;
        Add cache
    | Lit x, Add y ->
        let cache =
          new cache (fun _ ->
              print_endline "case 2";
              x#value + y#cache)
        in
        cache >> x;
        cache >> y;
        Add cache
    | Add x, Lit y ->
        let cache =
          new cache (fun _ ->
              print_endline "case 3";
              x#cache + y#value)
        in
        cache >> x;
        cache >> y;
        Add cache
    | Add x, Add y ->
        let cache =
          new cache (fun _ ->
              print_endline "case 4";
              x#cache + y#cache)
        in
        cache >> x;
        cache >> y;
        Add cache

  let ( + ) = add

  let ex =
    print_endline "OO implentation";
    let x = new state 3 in
    let y = new state 5 in
    let z = new state 8 in
    let w = new state 75 in

    let expr = Lit x + Lit y + (Lit z + Lit w) in
    let ppln expr =
      pp Format.std_formatter expr;
      Format.pp_print_newline Format.std_formatter ()
    in

    ppln expr;
    x#modify (fun x -> x * x * x);
    ppln expr;
    print_string "height: ";
    print_int (fromAdd expr)#height;
    print_newline ();
    ()
end
