open Cmdliner
open Util

let (</>) a b = Filename.concat a b
let log s = Printf.eprintf (s ^^ "\n%!")

let write_endpoints (_endpoints: Endpoints_t.endpoint list) = [Syntax.(
  let_ "endpoint_of"
    (fun_ "q"
      (app1 "print_endline" (ident "q"))))]


let main input outdir =
  log "Start processing endpoints";
  let inc = open_in input in
  let n = in_channel_length inc in
  let endpoint_data = really_input_string inc n in
  let endpoints = Endpoints_j.endpoints_of_string endpoint_data in
  endpoints |> List.iter (fun (name, (endpoints: Endpoints_t.endpoint list)) ->
    print_endline ("service: " ^ name);
    let outfile = outdir </> (name ^ "_endpoints.ml") in
    let syntax = write_endpoints endpoints in
    Printing.write_structure outfile syntax;
  );
  close_in inc;
  ()

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

let () =
  match Term.eval CommandLine.(gen_t, info) with
  | `Error _ -> exit 1
  | _        -> exit 0
