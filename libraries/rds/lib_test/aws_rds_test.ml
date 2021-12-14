open OUnit2
open Aws_rds

let from_opt = function
  | None -> assert false
  | Some x -> x

type config =
  { access_key : string
  ; secret_key : string
  ; region : string
  }

let ( @? ) = assert_bool

module TestSuite (Runtime : sig
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
end) =
struct
  let create_instance name config =
    Runtime.(
      un_m
        (run_request
           ~region:config.region
           ~access_key:config.access_key
           ~secret_key:config.secret_key
           (module CreateDBInstance)
           (Types.CreateDBInstanceMessage.make
              ~d_b_instance_identifier:name
              ~d_b_instance_class:"db.t3.micro"
              ~master_username:"master"
              ~master_user_password:"secret99"
              ~allocated_storage:20
              ~engine:"postgres"
              ())))

  let delete_instance d_b_instance_identifier config =
    Runtime.(
      un_m
        (run_request
           ~region:config.region
           ~access_key:config.access_key
           ~secret_key:config.secret_key
           (module DeleteDBInstance)
           (Types.DeleteDBInstanceMessage.make
              ~d_b_instance_identifier
              ~skip_final_snapshot:true
              ())))

  let describe_instances config =
    Runtime.(
      un_m
        (run_request
           ~region:config.region
           ~access_key:config.access_key
           ~secret_key:config.secret_key
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

  let create_rds_test config _ =
    let instance_name = "aws-test-instance-01" in
    let result = create_instance instance_name config in
    ("Creating RDS instance succeeds"
    @?
    match result with
    | `Ok _ -> true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false);
    Unix.sleep 30;
    let list_result = describe_instances config in
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
    let delete_result = delete_instance instance_name config in
    "Deleting RDS instance succeeds"
    @?
    match delete_result with
    | `Ok _ -> true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false

  let suite config =
    "Test RDS" >::: [ "RDS create / delete instance" >:: create_rds_test config ]

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
