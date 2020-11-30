open OUnit
open Aws_elasticache

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
    (* aws elasticache create-user \
     * --user-id user1 \
     * --user-name myUser \
     * --password mYnuUzrpAxXw2rdzx \
     * --engine redis \
     * --access-string "on ~app::* -@all +@read" *)

    let create_user () =
      Runtime.(
        un_m
          (run_request
             (module CreateUser)
             (Types.CreateUserMessage.make
                ~user_id:"user1"
                ~user_name:"myUser"
                ~engine:"redis"
                ~passwords:(Types.PasswordListInput.make [ "mYnuUzrpAxXw2rdzx" ] ())
                ~access_string:"on ~app::* -@all"
                ())))

    let describe_users () =
      Runtime.(
        un_m
          (run_request
             (module DescribeUsers)
             (Types.DescribeUsersMessage.make ~engine:"redis" ())))

    let delete_user () =
      Runtime.(
        un_m
          (run_request
             (module DeleteUser)
             (Types.DeleteUserMessage.make ~user_id:"user1" ())))

    let create_delete_user_test () =
      let request = create_user () in
      ("Elasticache create user"
      @?
      match request with
      | `Ok resp ->
          Printf.printf "User created \n";
          true
      | `Error err ->
          Printf.printf
            "Create Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false);
      ("Elasticache describe users"
      @?
      match describe_users () with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.pretty_to_string
               Types.DescribeUsersResult.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf
            "Describe Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false);
      let del_request = delete_user () in
      "Elasticache create user"
      @?
      match del_request with
      | `Ok resp ->
          Printf.printf "User deleted\n";
          true
      | `Error err ->
          Printf.printf
            "Delete Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false

    let test_cases = [ "ELASTICACHE create/delete user" >:: create_delete_user_test ]

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
