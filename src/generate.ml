(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Structures
open Util
open Migrate_parsetree
open Ast_404

let is_list ~shapes ~shp =
  try
    match (StringTable.find shp shapes).Shape.content with
    | Shape.List _ -> true
    | _            -> false
  with Not_found -> false

let is_flat_list ~shapes ~shp =
  try
    match (StringTable.find shp shapes).Shape.content with
    | Shape.List (_,_,true) -> true
    | _ -> false
  with Not_found -> false

let toposort (shapes : Shape.t StringTable.t) =
  let open Graph in
  let module G = Imperative.Digraph.ConcreteBidirectional(struct
      type t = Shape.t
      let compare a b = compare a.Shape.name b.Shape.name
      let hash a = Hashtbl.hash a.Shape.name
      let equal a b = a.Shape.name = b.Shape.name
    end) in
  let graph = G.create () in
  let add_edge from to_ =
    try
      G.add_edge graph from (StringTable.find to_ shapes)
    with Not_found -> ()
  in
  StringTable.iter (fun _key data ->
    G.add_vertex graph data;
    match data.Shape.content with
    | Shape.Structure members ->
      List.iter (fun mem -> add_edge data mem.Structure.shape) members
    | Shape.List (s,_,_) -> add_edge data s
    | Shape.Map ((ks,_), (vs,_)) -> add_edge data ks; add_edge data vs
    | Shape.Enum _ -> ())
  shapes;
  let module T = Topological.Make(G) in
  let out = ref [] in
  T.iter (fun shape -> out := shape :: !out) graph;
  !out


let types is_ec2 shapes =
  let option_type shp = function
    |true -> Syntax.ty0 shp
    |false -> Syntax.ty1 "option" shp in
  let build_module v =
    let mkrecty fs =
      Syntax.tyreclet "t" (List.map (fun (nm,shp,req) ->
          (nm, option_type shp req)) fs)
    in
    let ty =
      match v.Shape.content with
      | Shape.Structure [ ] ->
        (* Hack for a unit type since empty records aren't yet valid *)
        Syntax.tyunit "t"
      | Shape.Structure members ->
        mkrecty (List.map (fun m ->
                     (m.Structure.field_name, (String.capitalize_ascii m.Structure.shape) ^ ".t", m.Structure.required || is_list ~shapes ~shp:m.Structure.shape))
            members)
      | Shape.List (shp, _, _flatten) ->
        Syntax.tylet "t" (Syntax.ty1 "list" (shp ^ ".t"))
      | Shape.Map ((kshp, _loc), (vshp, _)) ->
        Syntax.tylet "t" (Syntax.ty2 "Hashtbl.t" (kshp ^ ".t") (vshp ^ ".t"))
      | Shape.Enum opts ->
        Syntax.tyvariantlet "t" (List.map (fun t -> (Util.to_variant_name t, [])) opts)
    in
    let make = match v.Shape.content with
      | Shape.Structure [ ] ->
        let body =
          Syntax.(fununit
                   (unit ())) in
        Syntax.let_ "make" body
      | Shape.Structure members ->
        let rec mkfun (args : Structure.member list) body = match args with
          | [ ] -> body
          | (x::xs) ->
            let fn = if x.Structure.required then Syntax.funlab else if is_list ~shapes ~shp:x.Structure.shape then Syntax.(funopt_def (list [])) else Syntax.funopt in
            fn x.Structure.field_name (mkfun xs body) in
        let body =
          Syntax.(fununit
                    (record
                       (List.map (fun a -> (a.Structure.field_name, ident a.Structure.field_name)) members))) in
        Syntax.let_ "make" (mkfun members body)
      | Shape.List _ -> [%stri let make elems () = elems]
      | Shape.Enum _ -> [%stri let make v () = v]
      (* TODO: maybe accept a list of tuples and create a Hashtbl *)
      | Shape.Map _ -> [%stri let make elems () = elems]
    in
    let extra = let open Syntax in match v.Shape.content with
      | Shape.Enum opts -> [let_ "str_to_t"
                              (list (List.map (fun o -> pair (str o) (ident (Util.to_variant_name o))) opts));
                            let_ "t_to_str"
                              (list (List.map (fun o -> pair (ident (Util.to_variant_name o)) (str o)) opts));
                            let_ "to_string"
                              (fun_ "e"
                                (app1 "Util.of_option_exn"
                                  (app2 "Util.list_find"
                                    (ident "t_to_str")
                                    (ident "e"))));
                            let_ "of_string"
                              (fun_ "s"
                                (app1 "Util.of_option_exn"
                                  (app2 "Util.list_find"
                                    (ident "str_to_t")
                                    (ident "s"))))
                           ]
      | _ -> []
    in
    let parse = match v.Shape.content with
      | Shape.Structure [ ] ->
        Syntax.(let_ "parse" (fun_ "xml" (app1 "Some" (unit ()))))
      | Shape.Structure s ->
        let fields = List.map (fun (mem : Structure.member) ->
            let loc_name =
              match mem.Structure.loc_name with
              | Some name -> name
              | None      -> mem.Structure.name
            in
            let b = if is_flat_list ~shapes ~shp:mem.Structure.shape then
              Syntax.(
                    (app1 (String.capitalize_ascii mem.Structure.shape ^ ".parse")
                    (ident "xml"))
              )
            else
              Syntax.(app2 "Util.option_bind"
                              (app2 "Xml.member" (str loc_name) (ident "xml"))
                              (ident (String.capitalize_ascii mem.Structure.shape ^ ".parse")))
            in
            let op =
              if mem.Structure.required then
                Syntax.(app2 "Xml.required" (str loc_name) b)
              else if is_list ~shapes ~shp:mem.Structure.shape then
                Syntax.(app2 "Util.of_option" (list []) b)
              else
                b
            in
            (mem.Structure.field_name, op))
            s
        in
        Syntax.(let_ "parse" (fun_ "xml" (app1 "Some" (record fields))))
      | Shape.Map ((_shp, _loc_name), _) ->
        Syntax.(let_ "parse"
                  (fun_ "xml"
                     (ident "None")))
      | Shape.List (shp, loc_name, _flatten) ->
        let item_name = match loc_name with
          | None -> "member"
          | Some nm -> nm in
            Syntax.(let_ "parse"
                      (fun_ "xml"
                         (app1 "Util.option_all"
                            (app2 "List.map"
                               (ident (shp ^ ".parse"))
                               (app2 "Xml.members" (str item_name) (ident "xml"))))))
      | Shape.Enum _opts ->
        Syntax.(let_ "parse"
                  (fun_ "xml"
                     (app2 "Util.option_bind"
                        (app1 "String.parse" (ident "xml"))
                        (fun_ "s"
                           (app2 "Util.list_find"
                              (ident "str_to_t")
                              (ident "s"))))))
    in
    let to_query = Syntax.(
        let_ "to_query"
          (fun_ "v"
             (match v.Shape.content with
              | Shape.Structure s ->
                (app1 "Query.List"
                   (app1 "Util.list_filter_opt"
                      (list (List.map (fun mem ->
                           let location =
                             match mem.Structure.loc_name with
                             | Some name -> name
                             | None      ->
                                mem.Structure.name ^
                               if not is_ec2 && is_list ~shapes ~shp:mem.Structure.shape
                               then ".member" else ""
                           in
                           let location = if is_ec2 then String.capitalize_ascii location else location in
                           let q arg =
                             app1 "Query.Pair"
                               (pair (str location) (app1 (String.capitalize_ascii mem.Structure.shape ^ ".to_query") arg)) in
                           (if mem.Structure.required || is_list ~shapes ~shp:mem.Structure.shape
                            then app1 "Some" (q (ident ("v." ^ mem.Structure.field_name)))
                            else app2 "Util.option_map" (ident ("v." ^ mem.Structure.field_name))
                                (fun_ "f" (q (ident "f"))))) s))))
              | Shape.List (shp,_,_flatten) ->
                (app2 "Query.to_query_list" (ident (shp ^ ".to_query")) (ident "v"))
              | Shape.Map ((key_shp,_),(val_shp,_)) ->
                (app3 "Query.to_query_hashtbl" (ident (key_shp ^ ".to_string")) (ident (val_shp ^ ".to_query")) (ident "v"))
              | Shape.Enum _ ->
                (app1 "Query.Value"
                   (app1 "Some"
                      (app1 "Util.of_option_exn"
                         (app2 "Util.list_find"
                            (ident "t_to_str")
                            (ident "v"))))))))
    in
    let to_json = Syntax.(
        let_ "to_json"
          (fun_ "v"
             (match v.Shape.content with
              | Shape.Structure s ->
                (variant1 "Assoc"
                   (app1 "Util.list_filter_opt"
                      (list
                         (List.map (fun mem ->
                              let q arg =
                                (pair
                                   (str mem.Structure.field_name)
                                   (app1 (String.capitalize_ascii mem.Structure.shape ^ ".to_json") arg)) in
                              if mem.Structure.required || is_list ~shapes ~shp:mem.Structure.shape
                              then app1 "Some" (q (ident ("v." ^ mem.Structure.field_name)))
                              else app2 "Util.option_map" (ident ("v." ^ mem.Structure.field_name))
                                  (fun_ "f" (q (ident "f")))

                            ) s))))
              | Shape.List (shp,_,_flatten) ->
                (variant1 "List"
                   (app2 "List.map"
                      (ident (shp ^ ".to_json"))
                      (ident "v")))
              | Shape.Map ((key_shp,_),(val_shp,_)) ->
                (variant1 "Assoc"
                   (app3 "Hashtbl.fold"
                      (fun3 "k" "v" "acc"
                         (list_expr (pair (app1 (key_shp ^ ".to_string") (ident "k")) (app1 (val_shp ^ ".to_json") (ident "v"))) (ident "acc")))
                      (ident "v")
                      (list [])))
              | Shape.Enum _ ->
                app1 "String.to_json"
                  (app1 "Util.of_option_exn"
                     (app2 "Util.list_find"
                        (ident "t_to_str")
                        (ident "v")))
             )))
    in
    let of_json = Syntax.(
        let_ "of_json"
          (fun_ "j"
             (match v.Shape.content with
              | Shape.Structure [ ] ->
                (* Hack for a unit type since empty records aren't yet valid *)
                Syntax.unit ()
              | Shape.Structure s ->
                record (List.map (fun mem ->
                    (mem.Structure.field_name,
                     (if mem.Structure.required || is_list ~shapes ~shp:mem.Structure.shape
                      then fun v -> app1 (String.capitalize_ascii mem.Structure.shape ^ ".of_json")
                          (app1 "Util.of_option_exn" v)
                      else fun v -> app2 "Util.option_map" v (ident (mem.Structure.shape ^ ".of_json")))
                       (app2 "Json.lookup" (ident "j") (str mem.Structure.field_name))))
                    s)
              | Shape.List (shp,_,_flatten) -> app2 "Json.to_list" (ident (shp ^ ".of_json")) (ident "j")
              | Shape.Map ((key_shp,_),(val_shp,_)) -> app3 "Json.to_hashtbl" (ident (key_shp ^ ".of_string")) (ident (val_shp ^ ".of_json")) (ident "j")
              | Shape.Enum _ ->
                (app1 "Util.of_option_exn"
                   (app2 "Util.list_find"
                      (ident "str_to_t")
                      (app1 "String.of_json" (ident "j"))))
             )))  in
    Syntax.module_ (String.capitalize_ascii v.Shape.name) ([ty] @ extra @ [make; parse; to_query; to_json; of_json]) in
  let modules = List.map build_module (toposort shapes) in
  let imports =
    [Syntax.open_ "Aws";
     Syntax.open_ "Aws.BaseTypes";
     Syntax.open_ "CalendarLib";
     Syntax.(tylet "calendar" (ty0 "Calendar.t"));
    ] in
  imports @ modules


let op service version _shapes op signature_version =
  let open Syntax in
  let mkty = function
    | None -> ty0 "unit"
    | Some shp -> ty0 (shp ^ ".t")
  in
  let defaults =
    (list [ pair (str "Action") (list [str op.Operation.name])
          ; pair (str "Version") (list [str version])
          ])
  in
  let to_body =

    letin "uri"
      (app2 "Uri.add_query_params"
         (app1 "Uri.of_string"
          (app1 "Aws.Util.of_option_exn" (app2 "Endpoints.url_of" (ident "service") (ident "region"))))
         (match op.Operation.input_shape with
          | None -> defaults
          | Some input_shape ->
            (app2 "List.append"
               defaults
               (app1 "Util.drop_empty"
                  (app1 "Uri.query_of_encoded"
                     (app1 "Query.render"
                        (app1 (input_shape ^ ".to_query") (ident "req"))))))))
      (tuple [variant op.Operation.http_meth; ident "uri"; list []])
  in
  let of_body =
    match op.Operation.output_shape with
    | None -> variant1 "Ok" (ident "()")
    | Some shp ->
      tryfail (letin "xml" (app1 "Ezxmlm.from_string" (ident "body"))
                 (letin "resp" (let r = app2 "Xml.member"
                                    (str (op.Operation.name ^ "Response"))
                                    (app1 "snd" (ident "xml")) in
                                match op.Operation.output_wrapper with
                                | None -> r
                                | Some w -> app2 "Util.option_bind"
                                              r (app1 "Xml.member" (str w)))
                    (try_msg "Xml.RequiredFieldMissing"
                       (app2 "Util.or_error"
                          (app2 "Util.option_bind"
                             (ident "resp")
                             (ident (shp ^ ".parse")))
                          (letom "Error"
                             (app1 "BadResponse"
                                (record [("body", ident "body"); ("message", str ("Could not find well formed " ^ shp ^ "."))]))))
                       (letom "Error"
                          (variant1 "Error"
                             (app1 "BadResponse"
                                (record [("body", ident "body"); ("message", app2 "^" (str ("Error parsing " ^ shp ^ " - missing field in body or children: ")) (ident "msg"))])))))
                 ))
        (variant1 "Error"
           (letom "Error"
              (app1 "BadResponse"
                 (record [("body", ident "body"); ("message", app2 "^"
                                                     (str "Error parsing xml: ")
                                                     (ident "msg")
                                                  )]))))
  in
    let op_error_parse =
    letin "errors" (app2 "@" (list (List.map (fun name -> ident ("Errors_internal." ^ (Util.to_variant_name name)))
                                      op.Operation.errors))
                      (ident "Errors_internal.common"))
      (matchoption (app1 "Errors_internal.of_string" (ident "err"))
         (ifthen (app2 "&&"
                    (app2 "List.mem" (ident "var") (ident "errors"))
                    (matchoption (app1 "Errors_internal.to_http_code" (ident "var"))
                       (app2 "=" (ident "var") (ident "code"))
                       (ident "true")))
            (app1 "Some" (ident "var"))
            (ident "None"))
         (ident "None"))
  in
  (** Tuple corresponding to (mli, ml) *)
  ([ sopen_ "Types"
   ; stylet "input" (mkty op.Operation.input_shape)
   ; stylet "output" (mkty op.Operation.output_shape)
   ; stylet "error" (ty0 "Errors_internal.t")
   ; sinclude_ "Aws.Call" [withty "input" "input"
                          ; withty "output" "output"
                          ; withty "error" "error"]
   ],
   [ open_ "Types"
   ; open_ "Aws"
   ; tylet "input" (mkty op.Operation.input_shape)
   ; tylet "output" (mkty op.Operation.output_shape)
   ; tylet "error" (ty0 "Errors_internal.t")
   ; let_ "service" (str service)
   ; let_ "signature_version" (ident ("Request." ^ String.capitalize_ascii signature_version))
   ; let_ "to_http" (fun3 "service" "region" "req" to_body)
   ; let_ "of_http" (fun_ "body" of_body)
   ; let_ "parse_error" (fun2 "code" "err" op_error_parse)
   ])


let errors errs common_errors =
  let errs = errs @ [Error.({shape_name = "UninhabitedError"; variant_name = "Uninhabited"; string_name = "Uninhabited"; http_code = None})] in
  let open Syntax in
  [ tyvariantlet "t"
      (List.map (fun e -> (e.Error.variant_name, [])) errs)
  ; let_ "common"
      (list (List.map (fun e -> ident e.Error.variant_name) common_errors))
  ; let_ "to_http_code"
      (fun_ "e"
         (matchvar (ident "e")
            (List.map (fun e ->
                 (e.Error.variant_name,
                  match e.Error.http_code with
                  | Some n -> app1 "Some" (int n)
                  | None -> ident "None"))
                errs)))
  ; let_ "to_string"
      (fun_ "e"
         (matchvar (ident "e")
         (List.map (fun e -> (e.Error.variant_name, str e.Error.string_name)) errs)))
  ; let_ "of_string"
      (fun_ "e"
         (matchstrs (ident "e")
            (List.map (fun e -> (e.Error.string_name, app1 "Some" (ident e.Error.variant_name))) errs)
            (ident "None")
         ))
  ]
