open OUnit
open Aws_rds

module TestSuite (Runtime : sig
  type 'a m

  val run_request :
       (module Aws.Call
          with type input = 'input
           and type output = 'output
           and type error = 'error)
    -> 'input
    -> [ `Ok of 'output | `Error of 'error Aws.Error.t ] m

  val un_m : 'a m -> 'a
end) =
struct
  let create_instance name =
    Runtime.(
      un_m
        (run_request
           (module CreateDBInstance)
           (Types.CreateDBInstanceMessage.make
              ~d_b_instance_identifier:name
              ~d_b_instance_class:"db.t3.micro"
              ~master_username:"master"
              ~master_user_password:"secret99"
              ~allocated_storage:20
              ~engine:"postgres"
              ())))

  let from_opt = function
    | None -> ""
    | Some x -> x

  let delete_instance d_b_instance_identifier =
    Runtime.(
      un_m
        (run_request
           (module DeleteDBInstance)
           (Types.DeleteDBInstanceMessage.make
              ~d_b_instance_identifier
              ~skip_final_snapshot:true
              ())))

  let describe_instances () =
    Runtime.(
      un_m
        (run_request
           (module DescribeDBInstances)
           (Types.DescribeDBInstancesMessage.make ())))

  let list_instances (x : Types.DBInstanceMessage.t) =
    List.iter
      (function
        | x ->
            let open Types.DBInstance in
            Printf.printf
              "%s \t %s \t %s"
              (from_opt x.d_b_instance_identifier)
              (from_opt x.d_b_instance_class)
              (from_opt x.engine))
      x.d_b_instances

  let create_rds_test () =
    let instance_name = "aws-test-instance-01" in
    let result = create_instance instance_name in
    ("Creating RDS instance succeeds"
    @?
    match result with
    | `Ok instance -> true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false);
    Unix.sleep 30;
    let list_result = describe_instances () in
    ("Describing RDS instances succeeds"
    @?
    match list_result with
    | `Ok resp ->
        Printf.printf "Instances: \n";
        list_instances resp;
        true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false);
    Unix.sleep 30;
    let delete_result = delete_instance instance_name in
    "Deleting RDS instance succeeds"
    @?
    match delete_result with
    | `Ok instance -> true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false

  let test_cases = [ "RDS create / delete instance" >:: create_rds_test ]

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
