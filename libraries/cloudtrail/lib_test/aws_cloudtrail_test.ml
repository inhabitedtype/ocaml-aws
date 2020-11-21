open OUnit
open Aws_cloudtrail

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
    (* Requires s3 bucket to already exist with correct Policy for cloudtrail.*)
    (* aws cloudtrail create-trail --name Trail1 --s3-bucket-name ocaml-aws-test-trail --is-multi-region-trail *)
    let create_trail () =
      Runtime.(
        un_m
          (run_request
             (module CreateTrail)
             (Types.CreateTrailRequest.make
                ~name:"ocaml-aws-test-trail"
                ~s3_bucket_name:"ocaml-aws-test-bucket"
                ~include_global_service_events:true
                ())))

    let delete_trail () =
      Runtime.(
        un_m
          (run_request
             (module DeleteTrail)
             (Types.DeleteTrailRequest.make ~name:"ocaml-aws-test-trail" ())))

    let from_opt = function
      | None -> "<no trail name>"
      | Some x -> x

    let create_delete_trail_test () =
      let create_result = create_trail () in
      ("Creating Cloudtrail"
      @?
      match create_result with
      | `Ok output ->
          Printf.printf "Success trail %s created.\n" (from_opt output.name);
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let delete_result = delete_trail () in
      "Deleting Cloudtrail"
      @?
      match delete_result with
      | `Ok output ->
          Printf.printf "Success trail deleted.\n";
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let test_cases = [ "Create delete trail" >:: create_delete_trail_test ]

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
