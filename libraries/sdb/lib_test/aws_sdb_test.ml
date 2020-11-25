open OUnit
open Aws_sdb

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
    let create_domain () =
      Runtime.(
        un_m
          (run_request
             (module CreateDomain)
             (Types.CreateDomainRequest.make ~domain_name:"ocaml-aws-domain-test" ())))

    let list_domains () =
      Runtime.(
        un_m
          (run_request
             (module ListDomains)
             (Types.ListDomainsRequest.make ~max_number_of_domains:2 ())))

    let delete_domain () =
      Runtime.(
        un_m
          (run_request
             (module DeleteDomain)
             (Types.DeleteDomainRequest.make ~domain_name:"ocaml-aws-domain-test" ())))

    let create_list_delete_test () =
      let create_request = create_domain () in
      ("Create Domain returns successfully"
      @?
      match create_request with
      | `Ok resp -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let list_request = list_domains () in
      ("List Domains returns successfully"
      @?
      match list_request with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.to_string
               Types.ListDomainsResult.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let delete_request = delete_domain () in
      "Delete Domain returns successfully"
      @?
      match delete_request with
      | `Ok resp -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let test_cases = [ "SDB create/list/delete" >:: create_list_delete_test ]

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
