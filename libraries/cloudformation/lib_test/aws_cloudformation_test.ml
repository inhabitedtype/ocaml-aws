open OUnit
open Aws_cloudformation

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

    let create_stack () =
      Runtime.(
        un_m
          (run_request
             (module CreateStack)
             (Types.CreateStackInput.make
                ~stack_name:"ocaml-aws-test-stack"
                ~template_u_r_l:
                  "https://s3.ap-southeast-2.amazonaws.com/cloudformation-templates-ap-southeast-2/Rails_Single_Instance.template"
                ~parameters:(Types.Parameters.make parameters ())
                ())))

    let delete_stack () =
      Runtime.(
        un_m
          (run_request
             (module DeleteStack)
             (Types.DeleteStackInput.make ~stack_name:"ocaml-aws-test-stack" ())))

    let create_stack_test () =
      let result = create_stack () in
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
      let delete_result = delete_stack () in
      "Delete Cloudformation stack"
      @?
      match delete_result with
      | `Ok output ->
          Printf.printf "Success stack deleted.\n";
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let test_cases = [ "Create cloudformation stack" >:: create_stack_test ]

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
