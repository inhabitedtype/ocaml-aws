open OUnit2
open Aws_sts

type config =
  { access_key : string
  ; secret_key : string
  ; region : string
  ; role_arn : string option
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
    let get_session_token config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module GetSessionToken)
             (Types.GetSessionTokenRequest.make ())))

    let assume_role config ?token () =
      let role_arn =
        match config.role_arn with
        | Some s -> s
        | None -> ""
      in
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             ?token
             (module AssumeRole)
             (Types.AssumeRoleRequest.make
                ~role_arn
                ~role_session_name:"ocaml-aws-test"
                ())))

    let get_session_token_test config _ =
      let result = get_session_token config () in
      "Get Session Token returns successfully"
      @?
      match result with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.to_string
               Types.GetSessionTokenResponse.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let print_assume_role_response resp =
      Printf.printf
        "%s\n"
        (Yojson.Basic.to_string
           Types.AssumeRoleResponse.(to_json (of_json (to_json resp))))

    let print_assume_role_error err =
      Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err)

    let assume_role_test config _ =
      skip_if
        (config.role_arn = None)
        "Environment variable AWS_STS_ROLE_ARN not available.";
      let result = assume_role config () in
      "Assume Role returns succcessfully"
      @?
      match result with
      | `Ok resp ->
          print_assume_role_response resp;
          true
      | `Error err ->
          print_assume_role_error err;
          false

    (* This test is designed to see if assume_role authentication works for
       `run_request`. Using a role ARN passed in via an environment variable,
       it gets an assume_role token for the user specified and then gets another token
       from those credentials. The role ARN passed in should have admin permissions in IAM. *)
    let assume_role_assume_role_test config _ =
      skip_if
        (config.role_arn = None)
        "Environment variable AWS_STS_ROLE_ARN not available.";
      let result = assume_role config () in
      "Assume Role able to Assume Role"
      @?
      match result with
      | `Ok resp -> (
          print_assume_role_response resp;
          match resp.credentials with
          | Some creds -> (
              let config2 =
                { config with
                  access_key = creds.access_key_id
                ; secret_key = creds.secret_access_key
                }
              in
              match assume_role config2 ~token:creds.session_token () with
              | `Ok resp2 ->
                  print_assume_role_response resp2;
                  true
              | `Error err ->
                  print_assume_role_error err;
                  false)
          | None ->
              Printf.eprintf "Did not receive credentials from first assume_role call.";
              false)
      | `Error err ->
          print_assume_role_error err;
          false

    let suite config =
      "Test STS"
      >::: [ "STS get_session_token" >:: get_session_token_test config
           ; "STS assume_role" >:: assume_role_test config
           ; "STS assume_role -> assume_role" >:: assume_role_assume_role_test config
           ]

    let () =
      let access_key =
        try Some (Unix.getenv "AWS_ACCESS_KEY_ID") with Not_found -> None
      in
      let secret_key =
        try Some (Unix.getenv "AWS_SECRET_ACCESS_KEY") with Not_found -> None
      in
      let region = try Some (Unix.getenv "AWS_DEFAULT_REGION") with Not_found -> None in
      let role_arn = try Some (Unix.getenv "AWS_STS_ROLE_ARN") with Not_found -> None in

      match access_key, secret_key, region with
      | Some access_key, Some secret_key, Some region ->
          run_test_tt_main (suite { access_key; secret_key; region; role_arn })
      | _, _, _ ->
          Printf.eprintf
            "Skipping running tests. Environment variables AWS_ACCESS_KEY_ID, \
             AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION not available. ";
          exit 0
  end
