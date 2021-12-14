open OUnit2
open Aws_elasticache

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
    (* aws elasticache create-user \
     * --user-id user1 \
     * --user-name myUser \
     * --password mYnuUzrpAxXw2rdzx \
     * --engine redis \
     * --access-string "on ~app::* -@all +@read" *)

    let create_user config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module CreateUser)
             (Types.CreateUserMessage.make
                ~user_id:"user1"
                ~user_name:"myUser"
                ~engine:"redis"
                ~passwords:(Types.PasswordListInput.make [ "mYnuUzrpAxXw2rdzx" ] ())
                ~access_string:"on ~app::* -@all"
                ())))

    let describe_users config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module DescribeUsers)
             (Types.DescribeUsersMessage.make ~engine:"redis" ())))

    let delete_user config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module DeleteUser)
             (Types.DeleteUserMessage.make ~user_id:"user1" ())))

    let create_delete_user_test config _ =
      let request = create_user config () in
      ("Elasticache create user"
      @?
      match request with
      | `Ok _ ->
          Printf.printf "User created \n";
          true
      | `Error err ->
          Printf.printf
            "Create Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false);
      ("Elasticache describe users"
      @?
      match describe_users config () with
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
      let del_request = delete_user config () in
      "Elasticache create user"
      @?
      match del_request with
      | `Ok _ ->
          Printf.printf "User deleted\n";
          true
      | `Error err ->
          Printf.printf
            "Delete Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false

    let suite config =
      "Test Elasticache" >::: [ "create/delete user" >:: create_delete_user_test config ]

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
