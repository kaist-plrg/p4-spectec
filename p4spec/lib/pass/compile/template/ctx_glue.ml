(* Per-instance context glue for generated OCaml.

   Replaces the old functor-internal state. The [iface]/[extern]/cache state of
   the current interpreter instance lives in a module-global [cur__], switched
   only at dispatch boundaries by [with_ctx] (save / set / restore), so towers of
   nested runners each keep their own context. *)

let static_head =
  {|
module H__ = struct
  type ('k, 'v) t = {
    data: ('k * 'v) list array;
    size: int;
  }
  let create n = { data = Array.make n []; size = n }
  let hash k = (Hashtbl.hash_param 100 1000 k) land max_int
  let find_opt h k =
    let b = (hash k) mod h.size in
    List.assoc_opt k h.data.(b)
  let replace h k v =
    let b = (hash k) mod h.size in
    h.data.(b) <- (k, v) :: List.filter (fun (k2,_) -> k2 <> k) h.data.(b)
  let clear h = Array.fill h.data 0 h.size []
end

type iface__ = {
  checkpoint    : unit -> int;
  seff          : int -> int -> bool;
  call_builtin  : (Value.t -> unit) -> Domain.Lib.Id.t -> Typ.t list -> Value.t list -> Value.t;
  parse_program : string list -> string list -> Run.parse_result;
}

type extern__ = {
  checkpoint       : unit -> int;
  seff             : int -> int -> bool;
  eval_extern_rel  : string -> Value.t list -> Run.rel_result;
  eval_extern_func : string -> Typ.t list -> Value.t list -> Run.func_result;
}
|}

let static_tail =
  {|
type ctx__ = {
  iface : iface__;
  extern : extern__;
  mutable cache_enabled : bool;
  caches : caches__;
}

let dummy_iface__ : iface__ = {
  checkpoint = (fun () -> failwith "spec_compiled: ctx not initialized");
  seff = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
  call_builtin = (fun _ _ _ _ -> failwith "spec_compiled: ctx not initialized");
  parse_program = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
}

let dummy_extern__ : extern__ = {
  checkpoint = (fun () -> failwith "spec_compiled: ctx not initialized");
  seff = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
  eval_extern_rel = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
  eval_extern_func = (fun _ _ _ -> failwith "spec_compiled: ctx not initialized");
}

let dummy__ : ctx__ = {
  iface = dummy_iface__;
  extern = dummy_extern__;
  cache_enabled = false;
  caches = make_caches__ ();
}

let cur__ : ctx__ ref = ref dummy__

let with_ctx (c : ctx__) (f : unit -> 'a) : 'a =
  let saved = !cur__ in
  cur__ := c;
  Fun.protect ~finally:(fun () -> cur__ := saved) f
|}

(* One typed [H__.t] field per cached function/relation, plus the record's
   constructor and reset. Driven by the cache entries collected during codegen. *)
let caches_section (entries : (Ml.id * Ml.typ * Ml.typ) list) : string =
  match entries with
  | [] ->
      "type caches__ = unit\n\
       let make_caches__ () : caches__ = ()\n\
       let clear_caches__ (_ : caches__) = ()\n"
  | _ ->
      let field (id, key_typ, val_typ) =
        Printf.sprintf "  %s : (%s, %s) H__.t;" id
          (Ml.Print.print_typ key_typ)
          (Ml.Print.print_typ val_typ)
      in
      let init (id, _, _) = Printf.sprintf "  %s = H__.create 4096;" id in
      let clear (id, _, _) = Printf.sprintf "  H__.clear c.%s;" id in
      let fields = entries |> List.map field |> String.concat "\n" in
      let inits = entries |> List.map init |> String.concat "\n" in
      let clears = entries |> List.map clear |> String.concat "\n" in
      Printf.sprintf
        "type caches__ = {\n\
         %s\n\
         }\n\n\
         let make_caches__ () : caches__ = {\n\
         %s\n\
         }\n\n\
         let clear_caches__ (c : caches__) =\n\
         %s\n\
        \  ()\n"
        fields inits clears

let glue (entries : (Ml.id * Ml.typ * Ml.typ) list) : string =
  static_head ^ "\n" ^ caches_section entries ^ "\n" ^ static_tail
