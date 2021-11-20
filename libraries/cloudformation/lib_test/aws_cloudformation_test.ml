open OUnit2
open Aws_cloudformation

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
    let parameters =
      [ Types.Parameter.make ~parameter_key:"KeyName" ~parameter_value:"??" ()
      ; Types.Parameter.make ~parameter_key:"DBName" ~parameter_value:"railsdb" ()
      ; Types.Parameter.make ~parameter_key:"DBUser" ~parameter_value:"master" ()
      ; Types.Parameter.make ~parameter_key:"DBPassword" ~parameter_value:"password" ()
      ; Types.Parameter.make ~parameter_key:"DBPassword" ~parameter_value:"password" ()
      ; Types.Parameter.make
          ~parameter_key:"DBRootPassword"
          ~parameter_value:"password"
          ()
      ; Types.Parameter.make ~parameter_key:"InstanceType" ~parameter_value:"t1.micro" ()
      ; Types.Parameter.make ~parameter_key:"SSHLocation" ~parameter_value:"0.0.0.0/0" ()
      ]

    let from_opt = function
      | None -> "<no stack id>"
      | Some x -> x

    let create_stack config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module CreateStack)
             (Types.CreateStackInput.make
                ~stack_name:"ocaml-aws-test-stack"
                ~template_u_r_l:
                  "https://s3.ap-southeast-2.amazonaws.com/cloudformation-templates-ap-southeast-2/Rails_Single_Instance.template"
                ~parameters:(Types.Parameters.make parameters ())
                ())))

    let delete_stack config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module DeleteStack)
             (Types.DeleteStackInput.make ~stack_name:"ocaml-aws-test-stack" ())))

    let create_stack_test config _ =
      let result = create_stack config () in
      ("Creating Cloudformation stack - ruby on rails flavoured!"
      @?
      match result with
      | `Ok output ->
          Printf.printf "Success stack %s created.\n" (from_opt output.stack_id);
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      Unix.sleep 30;
      let delete_result = delete_stack config () in
      "Delete Cloudformation stack"
      @?
      match delete_result with
      | `Ok _ ->
          Printf.printf "Success stack deleted.\n";
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let suite config =
      "Test Cloudformation"
      >::: [ "Create cloudformation stack" >:: create_stack_test config ]

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
             AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION not available. ";
          exit 0
  end
