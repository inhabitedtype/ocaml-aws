open OUnit
open Aws_ssm

module type Runtime = sig
  type 'a m

  val run_request :
       (module Aws.Call
          with type input = 'input
           and type output = 'output
           and type error = 'error)
    -> 'input
    -> [ `Ok of 'output | `Error of 'error Aws.Error.t ] m

  val un_m : 'a m -> 'a
end

module TestSuite =
functor
  (Runtime : Runtime)
  ->
  struct
    (* aws ssm create-document \
     * --content file://exampleDocument.yml \
     * --name "Example" \
     * --document-type "Automation" \
     *                 --document-format YAML *)
    let create_document ~name () =
      Runtime.(
        un_m
          (run_request
             (module CreateDocument)
             (Types.CreateDocumentRequest.make ~content:"" ~name ())))

    let list_documents () =
      Runtime.(
        un_m
          (run_request
             (module ListDocuments)
             (Types.ListDocumentsRequest.make ~max_results:10 ())))

    let delete_document ~name () =
      Runtime.(
        un_m
          (run_request
             (module DeleteDocument)
             (Types.DeleteDocumentRequest.make ~name ())))

    let list_test () =
      let list_request = list_documents () in
      "List Documents returns successfully"
      @?
      match list_request with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.to_string
               Types.ListDocumentsResult.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let test_cases = [ "SSM list test" >:: list_test ]

    let rec was_successful = function
      | [] -> true
      | RSuccess _ :: t | RSkip _ :: t -> was_successful t
      | RFailure _ :: _ | RError _ :: _ | RTodo _ :: _ -> false

    let _ =
      let suite = "Tests" >::: test_cases in
      let verbose = ref false in
      let set_verbose _ = verbose := true in
      Arg.parse
        [ "-verbose", Arg.Unit set_verbose, "Run the test in verbose mode." ]
        (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
        ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
      if not (was_successful (run_test_tt ~verbose:!verbose suite)) then exit 1
  end
