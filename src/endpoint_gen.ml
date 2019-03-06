open Cmdliner
open Util

let (</>) a b = Filename.concat a b
let log s = Printf.eprintf (s ^^ "\n%!")

(** [filter_missing] removes any [None] elements from the provided list. *)
let filter_missing =
  List.fold_left
    (fun a  -> function | None  -> a | Some s -> List.append [s] a) []

(** [endpoint_str_matches uri ss] creates the match structure for regions that
    match particular strings, since matchstr already has an [els] clause, this doesn't
    require it. *)
let endpoint_str_matches uri ss =
  let open Syntax in
    let ds = ss |> filter_missing in
    ds |> (List.map (fun s -> (s, (app1 "Some" (str uri)))))

(** [endpoint_str_not_matches uri ss els] creates the match structure for regions that
    do NOT match particular strings, including null. This is somewhat tricky and generates
    naive code that can never be reached *)
let endpoint_str_not_matches uri ss els = Syntax.(
  let m = app1 "Some" (str uri) in
  let matches = ss |> List.map (function
                                 | None  -> ("region", m)
                                 | Some s -> (("Some " ^ s), m)) in
  matchvar (ident "region") (List.append matches [("_", els)])
)

(** [endpoint_starts_with uri ss] creates the match structure for regions that
    match particular string *)
let endpoint_starts_with uri ss els =
  let open Syntax in
    let ds = ss |> filter_missing in
    ds |>
      (List.fold_left
         (fun a  ->
            fun s  ->
              ifthen (app2 "Aws.Util.str_starts_with" (ident "region") (str s))
                (app1 "Some" (str uri)) a) els)

(** [write_constraints uri cs els] emits the syntax for constraints provided
    by the _endpoints.json file for a given uri. Currently, the only kind of
    constraints are against [`REGION] so we ignore that for now. *)
let write_constraints uri (cs : Endpoints_t.constraint_ list) els =
  let open Syntax in
    List.fold_left
      (fun a  ->
         function
         | (_on,`NOT_EQUALS,d) ->
             endpoint_str_not_matches uri Endpoints_t.(d.data) a
         | (_on,`EQUALS,d) ->
             matchstrs (ident "region")
               (endpoint_str_matches uri Endpoints_t.(d.data)) a
         | (_on,`STARTS_WITH,d) ->
             endpoint_starts_with uri Endpoints_t.(d.data) a
         | _ -> a) els cs

(** [write_endpoints endpoints] takes a list of [Endpoint_t.endpoint] and
    generates the syntax for the service endpoint matching defined by the
    _endpoints.json file
*)
let write_endpoints (endpoints : Endpoints_t.endpoint list) =
  [let open Syntax in
     let_ "endpoint_of"
       (fun_ "region"
          (endpoints |> List.rev |>
              (List.fold_left (fun a ->
                 (fun (e : Endpoints_t.endpoint)  ->
                    match e.constraints with
                    | None  -> ident e.uri
                    | Some cs -> write_constraints e.uri cs a)) (ident "None")))
             )]

let main input outdir =
  log "Start processing endpoints";

  let inc = open_in input in
  let n = in_channel_length inc in
  let endpoint_data = really_input_string inc n in
  let endpoints = Endpoints_j.endpoints_of_string endpoint_data in
  endpoints |> List.iter
        (fun (name,(endpoints : Endpoints_t.endpoint list))  ->
           print_endline ("service: " ^ name);
           (match name with
            | "sqs" ->
                let outfile = (outdir </> name) </> (name ^ "_endpoints.ml") in
                let syntax = write_endpoints endpoints in
                Printing.write_structure outfile syntax
            | _ -> print_endline "not writing temporarily"));
   close_in inc;

module CommandLine = struct
  let input =
    let doc = "JSON file specifying AWS endpoints to generate" in
    Arg.(required & opt (some non_dir_file) None & info ["i"; "input-file"] ~docv:"Filename" ~doc)

  let outdir =
    let doc = "directory where generated library should be put" in
    Arg.(required & opt (some dir) None & info ["o"; "output-directory"] ~docv:"Directory" ~doc)

  let gen_t = Term.(pure main $ input $ outdir)

  let info =
    let doc = "Generate the endpoints mapping for AWS resources." in
    Term.info "endpoint-gen" ~version:"0.0.1" ~doc
end

(** entrypoint *)
let () =
  match Term.eval CommandLine.(gen_t, info) with
  | `Error _ -> exit 1
  | _        -> exit 0
