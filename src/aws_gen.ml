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
  String.map
    (fun c ->
      if !at_start
      then (
        at_start := false;
        Char.lowercase_ascii c)
      else c)
    s

module Json = struct
  include Yojson.Basic.Util

  let member_exn name json =
    let r = Yojson.Basic.Util.member name json in
    if r = `Null then raise Not_found else r

  let lookup_list field assoc = try to_list (List.assoc field assoc) with _ -> []

  let rec merge (orig : Yojson.Basic.t) (extra : Yojson.Basic.t) : Yojson.Basic.t =
    match orig, extra with
    | `Assoc os, `Assoc es ->
        let upd_ =
          List.map
            (fun (ok, ov) ->
              try ok, merge ov (List.assoc ok es) with Not_found -> ok, ov)
            os
        in
        let new_ = List.filter (fun (ek, _) -> not (List.mem_assoc ek os)) es in
        `Assoc (upd_ @ new_)
    | _, v -> v

  let update_with_default assoc ~key ~f ~default =
    let updated = ref false in
    let result =
      List.map
        (fun (key', val_) ->
          if key = key'
          then (
            updated := true;
            key, `List (f val_))
          else key', val_)
        assoc
    in
    if not !updated then (key, default) :: assoc else result

  let override_shapes original overrides : (string * Yojson.Basic.t) list =
    let open Yojson.Basic.Util in
    List.map
      (fun (key, val_) ->
        try
          let shapeOverrides = to_assoc (List.assoc key overrides) in
          let required = lookup_list "requiredFields" shapeOverrides in
          let optional = lookup_list "optionalFields" shapeOverrides in
          let f reqs =
            List.(append (filter (fun x -> not (mem x optional)) (to_list reqs)) required)
          in
          let default = `List required in
          key, `Assoc (update_with_default (to_assoc val_) ~key:"required" ~f ~default)
        with Not_found -> key, val_)
      original

  let override original overrides =
    let open Yojson.Basic.Util in
    let original = to_assoc original in
    let overrides = to_assoc overrides in
    `Assoc
      (List.map
         (fun (key, val_) ->
           if key = "shapes"
           then
             let types =
               try to_assoc (List.assoc "typeOverrides" overrides) with _ -> []
             in
             key, `Assoc (override_shapes (to_assoc val_) types)
           else key, val_)
         original)
end

let log s = Printf.eprintf (s ^^ "\n%!")

let ( </> ) a b = Filename.concat a b

let rec mkdir_p ?(root = "") dirs =
  match dirs with
  | [] -> ()
  | d :: ds ->
      let dir = root </> d in
      (try Unix.mkdir dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      mkdir_p ~root:dir ds

let main input override errors_path outdir is_ec2 (optional_libs : string list) =
  log "## Generating...";
  let overrides =
    match override with
    | None -> `Assoc []
    | Some over -> Yojson.Basic.from_file over
  in
  let desc =
    let json = Yojson.Basic.from_file input in
    Json.override json overrides
  in
  (* Override endpoint names for particular services. *)
  let lib_name' meta =
    match Json.(member_exn "endpointPrefix" meta |> to_string) with
    | "monitoring" -> "cloudwatch"
    | a -> a
  in

  let meta = Json.(member_exn "metadata" desc) in
  let ops_json = Json.(member_exn "operations" desc |> to_assoc) in
  let shp_json = Json.(member_exn "shapes" desc |> to_assoc) in
  let lib_name = Json.(member_exn "endpointPrefix" meta |> to_string) in
  let lib_name_dir = lib_name' meta in
  let service_name = Json.(member_exn "serviceFullName" meta |> to_string) in
  let api_version = Json.(member_exn "apiVersion" meta |> to_string) in
  let signature_version = Json.(member_exn "signatureVersion" meta |> to_string) in
  let parsed_ops = List.map Reading.op ops_json in
  let common_errors =
    let parse_common common =
      Util.filter_map (Json.to_list common) ~f:(fun e ->
          try
            let error = Json.member_exn "error" e in
            let code = Json.(member_exn "code" error |> to_string) in
            Some (Reading.error code e)
          with _ -> None)
    in
    match errors_path with
    | None -> []
    | Some path ->
        let json = Yojson.Basic.from_file path in
        let common =
          match Json.member "common" json with
          | `Null -> []
          | common -> parse_common common
        in
        let specific =
          match Json.member "specific" json with
          | `Null -> []
          | specific -> (
              match Json.member lib_name specific with
              | `Null -> []
              | specific -> parse_common specific)
        in
        common @ specific
  in
  let errors =
    List.sort_uniq
      Structures.Error.compare
      (Util.filter_map shp_json ~f:(fun (nm, flds) ->
           match Json.member "exception" flds with
           | `Bool true -> Some (Reading.error nm flds)
           | `Null | `Bool false -> None
           | _ -> assert false)
      @ common_errors)
  in
  let shapes, ops =
    inline_shapes
      parsed_ops
      (List.fold_left
         (fun acc j ->
           let ((nm, _, _) as shape) = Reading.shape j in
           StringTable.add nm shape acc)
         StringTable.empty
         shp_json)
  in
  mkdir_p [ outdir; lib_name_dir; "lib" ];
  mkdir_p [ outdir; lib_name_dir; "lib_test" ];
  let dir = outdir </> lib_name_dir in
  let lib_dir = dir </> "lib" in
  let lib_dir_test = dir </> "lib_test" in
  Printing.write_structure (lib_dir </> "types.ml") (Generate.types is_ec2 shapes);
  log
    "## Wrote %d/%d shape modules..."
    (StringTable.cardinal shapes)
    (List.length shp_json);
  Printing.write_structure
    (lib_dir </> "errors_internal.ml")
    (Generate.errors errors common_errors);
  log "## Wrote %d error variants..." (List.length errors);
  List.iter
    (fun op ->
      let mli, ml = Generate.op lib_name api_version shapes op signature_version in
      let modname = uncapitalize op.Operation.name in
      Printing.write_signature (lib_dir </> modname ^ ".mli") mli;
      Printing.write_structure (lib_dir </> modname ^ ".ml") ml)
    ops;
  log "## Wrote %d/%d ops modules..." (List.length ops) (List.length ops_json);
  Printing.write_all
    ~filename:(lib_dir </> "dune")
    (Templates.dune ~lib_name:lib_name_dir ~service_name);
  log "## Wrote dune file.";
  Printing.write_all
    ~filename:(lib_dir_test </> "dune")
    (Templates.dune_test ~optional_libs ~lib_name:lib_name_dir ());

  log "## Wrote test runner files.";
  Printing.write_all
    ~filename:(lib_dir_test </> "test_async.ml")
    (Templates.test_async ~lib_name:lib_name_dir);
  Printing.write_all
    ~filename:(lib_dir_test </> "test_lwt.ml")
    (Templates.test_lwt ~lib_name:lib_name_dir);

  (* TODO Only overwrite if it doesn't already exist *)
  (* Printing.write_all ~filename:(lib_dir_test </> ("aws_" ^ lib_name_dir ^ "_test.ml"))
   *   (Templates.service_test ~lib_name:lib_name_dir); *)
  log "## Wrote opam file";
  Printing.write_all
    ~filename:("./" </> "aws-" ^ lib_name_dir ^ ".opam")
    (Templates.opam ~service_name)

module CommandLine = struct
  let input =
    let doc = "JSON file specifying library to generate" in
    Arg.(
      required
      & opt (some non_dir_file) None
      & info [ "i"; "input-file" ] ~docv:"Filename" ~doc)

  let override =
    let doc = "JSON file with overrides over input JSON" in
    Arg.(
      value
      & opt (some non_dir_file) None
      & info [ "r"; "override" ] ~docv:"Filename" ~doc)

  let outdir =
    let doc = "directory where generated library should be put" in
    Arg.(
      required
      & opt (some dir) None
      & info [ "o"; "output-directory" ] ~docv:"Directory" ~doc)

  let errors =
    let doc =
      "JSON file with common and specific errors unspecified in service description"
    in
    Arg.(
      value & opt (some non_dir_file) None & info [ "e"; "errors" ] ~docv:"Filename" ~doc)

  let is_ec2 =
    let doc = "This enables EC2-specific special casing in parts of code generation." in
    Arg.(value & flag & info [ "is-ec2" ] ~docv:"Filename" ~doc)

  let optional_libs =
    let doc = "This allows us to include arbitrary libraries in our test files" in
    let type_ = Arg.(opt (list string) []) in
    Arg.(value & (type_ @@ info ~docv:"Filename" [ "optional-libs" ] ~doc))

  let gen_t =
    Term.(pure main $ input $ override $ errors $ outdir $ is_ec2 $ optional_libs)

  let info =
    let doc = "Generate a library for an AWS schema." in
    Term.info "aws_gen" ~version:"0.0.1" ~doc
end

let () =
  match Term.eval CommandLine.(gen_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
