open OUnit2
open Aws_cloudtrail

let from_opt = function
  | None -> "<no trail name>"
  | Some x -> x

type config =
  { access_key : string
  ; secret_key : string
  ; region : string
  }

let ( @? ) = assert_bool

module type Runtime = sig
  type 'a m

  val run_request :
       region:string
    -> access_key:string
    -> secret_key:string
    -> ?token:string
    -> (module Aws.Call
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
    let create_trail config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module CreateTrail)
             (Types.CreateTrailRequest.make
                ~name:"ocaml-aws-test-trail"
                ~s3_bucket_name:"ocaml-aws-test-bucket"
                ~include_global_service_events:true
                ())))

    let delete_trail config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module DeleteTrail)
             (Types.DeleteTrailRequest.make ~name:"ocaml-aws-test-trail" ())))

    let create_delete_trail_test config _ =
      let create_result = create_trail config () in
      ("Creating Cloudtrail"
      @?
      match create_result with
      | `Ok output ->
          Printf.printf "Success trail %s created.\n" (from_opt output.name);
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let delete_result = delete_trail config () in
      "Deleting Cloudtrail"
      @?
      match delete_result with
      | `Ok _ ->
          Printf.printf "Success trail deleted.\n";
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let suite config =
      "Test Cloudtrail" >::: [ "Create delete trail" >:: create_delete_trail_test config ]

    let () =
      let access_key =
        try Some (Unix.getenv "AWS_ACCESS_KEY_ID") with Not_found -> None
      in
      let secret_key =
        try Some (Unix.getenv "AWS_SECRET_ACCESS_KEY") with Not_found -> None
      in
      let region = try Some (Unix.getenv "AWS_DEFAULT_REGION") with Not_found -> None in

      match access_key, secret_key, region with
      | Some access_key, Some secret_key, Some region ->
          run_test_tt_main (suite { access_key; secret_key; region })
      | _, _, _ ->
          Printf.eprintf
            "Skipping running tests. Environment variables AWS_ACCESS_KEY_ID, \
             AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION not available.";
          exit 0
  end
