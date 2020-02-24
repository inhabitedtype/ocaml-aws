open OUnit
open Aws_rds

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
         (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let create_instance name =
    Runtime.(un_m (run_request (module CreateDBInstance)
                     (Types.CreateDBInstanceMessage.make
                        ~d_b_instance_identifier:name
                        ~d_b_instance_class:"db.t2.micro"
                        ~master_username:"root"
                        ~master_user_password:"root-password"
                        ~allocated_storage:10
                        ~engine:"postgres" ())))

  let delete_instance d_b_instance_identifier =
    Runtime.(un_m (run_request (module DeleteDBInstance)
                     (Types.DeleteDBInstanceMessage.make ~d_b_instance_identifier ())))

  let create_rds_test () =
    let result = create_instance "aws-test-instance" in
    "Creating RDS instance succeeds"
    @? begin match result with
       | `Ok instance -> true
       | `Error _ -> false
       end;
    Unix.sleep 3;
    let delete_result = delete_instance "aws-test-instance" in
    "Deleting RDS instance succeeds"
    @? begin match delete_result with
       | `Ok instance -> true
       | `Error _ -> false
       end

  let test_cases =
    [ "RDS create / delete instance" >:: create_rds_test ]

  let rec was_successful =
    function
    | [] -> true
    | RSuccess _::t
      | RSkip _::t ->
       was_successful t
    | RFailure _::_
      | RError _::_
      | RTodo _::_ ->
       false

  let _ =
    let suite = "Tests" >::: test_cases in
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
      exit 1
end
