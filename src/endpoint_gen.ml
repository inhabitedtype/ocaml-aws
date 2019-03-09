open Cmdliner

let (</>) a b = Filename.concat a b
let log s = Printf.eprintf (s ^^ "\n%!")

let print_partition (p : Endpoints_t.partition) =
  print_endline ("dns_suffix: " ^ p.dns_suffix);
  print_endline ("partition: " ^ p.partition);
  print_endline ("partition_name: " ^ p.partition_name);;

let var_replace hostname service_name region dns_suffix =
  let hostname = Str.replace_first (Str.regexp_string {|{region}|}) region hostname in
  let hostname = Str.replace_first (Str.regexp_string {|{service}|}) service_name hostname in
  Str.replace_first (Str.regexp_string {|{dnsSuffix}|}) dns_suffix hostname

let write_endpoint
  region
  dns_suffix
  (default_hostname : string option)
  ((service_name, endpoint) : (string * Endpoints_t.endpoint)) = Syntax.(
  let host = match (endpoint.hostname, default_hostname) with
    | (None, None) -> (ident "None")
    | (None, Some(h))
    | (Some(h), _) -> (app1 "Some" (str (var_replace h service_name region dns_suffix))) in
  (service_name, host)
)

let write_service
  dns_suffix
  (partition_defaults : Endpoints_t.partition_defaults)
  ((region, svc) : (string * Endpoints_t.service)) = Syntax.(
  (region, (matchstrs
    (ident "region")
    (svc.endpoints |> List.map (write_endpoint region dns_suffix partition_defaults.hostname))
    (ident "None")))
)

let write_partition (p : Endpoints_t.partition) = Syntax.(
  let_ "endpoint_of"
    (fun2 "svc_name" "region"
      (matchstrs
        (ident "svc_name")
        (p.services |> List.map (write_service p.dns_suffix p.defaults))
        (ident "None")))
)

let main input outdir =
  log "Start processing endpoints";

  let inc = open_in input in
  let n = in_channel_length inc in
  let endpoint_data = really_input_string inc n in
  let endpoints = Endpoints_j.endpoints_of_string endpoint_data in
  let aws = endpoints.partitions
    |> List.find (fun p -> String.equal Endpoints_t.(p.partition) "aws") in
  let outfile = (outdir </> "Aws_endpoints.ml") in
  let syntax = write_partition aws in
  Util.Printing.write_structure outfile [syntax];
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
