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

open Cmdliner

open Structures
open Util

let uncapitalize s =
  let at_start = ref true in
  String.map (fun c ->
    if !at_start then
      begin at_start := false; Char.lowercase c end
    else c)
  s

module Json = struct
  include Yojson.Basic.Util

  let member_exn name json =
    let r = Yojson.Basic.Util.member name json in
    if r = `Null then raise Not_found else r

  let lookup_list field assoc =
    try to_list (List.assoc field assoc) with _ -> []

  let rec merge (orig:Yojson.Basic.json) (extra:Yojson.Basic.json) : Yojson.Basic.json =
    match orig, extra with
    | `Assoc os, `Assoc es ->
      let upd_ = List.map (fun (ok, ov) ->
        try (ok, merge ov (List.assoc ok es)) with Not_found -> (ok, ov)) os in
      let new_ = List.filter (fun (ek, _) -> not (List.mem_assoc ek os)) es in
      `Assoc (upd_ @ new_)
    | _, v -> v

  let update_with_default assoc ~key ~f ~default =
    let updated = ref false in
    let result = List.map (fun (key', val_) ->
      if key = key'
        then begin updated := true; (key, `List (f val_)) end
        else (key', val_))
      assoc
    in
    if not !updated
      then (key, default) :: assoc
      else result
  ;;

  let override_shapes original overrides : (string * Yojson.Basic.json) list =
    let open Yojson.Basic.Util in
    List.map (fun (key, val_) ->
      try
        let shapeOverrides = to_assoc (List.assoc key overrides) in
        let required = lookup_list "requiredFields" shapeOverrides in
        let optional = lookup_list "optionalFields" shapeOverrides in
        let f reqs =
          List.(append (filter (fun x -> not (mem x optional)) (to_list reqs)) required)
        in
        let default = `List required in
        (key, `Assoc (update_with_default (to_assoc val_) ~key:"required" ~f ~default))
      with Not_found -> (key, val_))
    original

  let override original overrides =
    let open Yojson.Basic.Util in
    let original  = to_assoc original in
    let overrides = to_assoc overrides in
    `Assoc (List.map (fun (key, val_) ->
      if key = "shapes" then
        let types = try to_assoc (List.assoc "typeOverrides" overrides) with _ -> [] in
        (key, `Assoc (override_shapes (to_assoc val_) types))
      else
        (key, val_))
    original)
end

let main input override errors_path outdir is_ec2 =
  print_endline "## Generating...";
  let desc =
    let json = Yojson.Basic.from_file input in
    match override with
    | None      -> json
    | Some over -> Json.override json (Yojson.Basic.from_file over)
  in
  let meta     = Json.(member_exn "metadata"       desc) in
  let service  = Json.(member_exn "endpointPrefix" meta |> to_string) in
  let version  = Json.(member_exn "apiVersion"     meta |> to_string) in
  let ops_json = Json.(member_exn "operations"     desc |> to_assoc) in
  let shp_json = Json.(member_exn "shapes"         desc |> to_assoc) in
  let parsed_ops = List.map Reading.op ops_json in
  let common_errors =
    let parse_common common =
      Util.filter_map (Json.to_list common) ~f:(fun e ->
        try
          let error = Json.member_exn "error" e in
          let code  = Json.(member_exn "code"  error |> to_string) in
          Some (Reading.error code e)
        with _ -> None)
    in
    match errors_path with
    | None -> []
    | Some path ->
      let json = Yojson.Basic.from_file path in
      let common =
        match Json.member "common" json with
        | `Null  -> []
        | common -> parse_common common
      in
      let specific =
        match Json.member "specific" json with
        | `Null    -> []
        | specific ->
          begin match Json.member service specific with
          | `Null -> []
          | specific -> parse_common specific
          end
      in
      common @ specific
  in
  let errors =
    List.sort_uniq Pervasives.compare (Util.filter_map shp_json ~f:(fun (nm, flds) ->
      match Json.member "exception" flds with
      | `Bool true -> Some(Reading.error nm flds)
      | `Null | `Bool false -> None
      | _ -> assert false)
    @ common_errors)
  in
  let (shapes, ops) = inline_shapes parsed_ops
    (List.fold_left (fun acc j ->
      let (nm, _, _) as shape = Reading.shape j in
      StringTable.add nm shape acc)
    StringTable.empty shp_json)
  in
  let dir = outdir ^ "/" ^ service in
  mkdir_safe dir;
  mkdir_safe (dir ^ "/lib");
  Printing.write_structure (dir ^ "/lib/types.ml") (Generate.types is_ec2 shapes);
  Printf.printf "## Wrote %d/%d shape modules...\n"
    (StringTable.cardinal shapes) (List.length shp_json);
  Printing.write_structure (dir ^ "/lib/errors.ml") (Generate.errors errors common_errors);
  Printf.printf "## Wrote %d error variants...\n" (List.length errors);
  List.iter (fun op ->
    let (mli, ml) = Generate.op service version shapes op in
    let modname = uncapitalize op.Operation.name in
    Printing.write_signature (dir ^ "/lib/" ^ modname ^ ".mli") mli;
    Printing.write_structure (dir ^ "/lib/" ^ modname ^ ".ml") ml)
  ops;
  Printf.printf "## Wrote %d/%d ops modules...\n"
    (List.length ops) (List.length ops_json);
  let mods = List.map (fun op -> op.Operation.name) ops in
  let oasis_append =
    try
      let in_ = open_in (dir ^ "/_oasis_append") in
      really_input_string in_ (in_channel_length in_)
    with Sys_error _ -> ""
  in
  Util.Printing.write_all (dir ^ "/_oasis") (Templates.oasis oasis_append service mods);
  print_endline "## Wrote _oasis file.";
;;

module CommandLine = struct
  let input =
    let doc = "JSON file specifying library to generate" in
    Arg.(required & opt (some non_dir_file) None & info ["i"; "input-file"] ~docv:"Filename" ~doc)

  let override =
    let doc = "JSON file with overrides over input JSON" in
    Arg.(value & opt (some non_dir_file) None & info ["r"; "override"] ~docv:"Filename" ~doc)

  let outdir =
    let doc = "directory where generated library should be put" in
    Arg.(required & opt (some dir) None & info ["o"; "output-directory"] ~docv:"Directory" ~doc)

  let errors =
    let doc = "JSON file with common and specific errors unspecified in service description" in
    Arg.(value & opt (some non_dir_file) None & info ["e"; "errors"] ~docv:"Filename" ~doc)

  let is_ec2 =
    let doc = "This enables EC2-specific special casing in parts of code generation." in
    Arg.(value & flag & info ["is-ec2"] ~docv:"Filename" ~doc)

  let gen_t = Term.(pure main $ input $ override $ errors $ outdir $ is_ec2)

  let info =
    let doc = "Generate a library for an AWS schema." in
    Term.info "aws_gen" ~version:"0.0.1" ~doc
end

let () =
  match Term.eval CommandLine.(gen_t, info) with
  | `Error _ -> exit 1
  | _        -> exit 0
