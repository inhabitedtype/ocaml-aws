open OUnit2
open Aws_ec2

let from_opt = function
  | None -> assert false
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
    let describe_regions_json config _ =
      let res =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module DescribeRegions)
               (Types.DescribeRegionsRequest.make ())))
      in
      "DescribeRegions returns values, and to_json / from_json round trips them"
      @?
      match res with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.pretty_to_string
               Types.DescribeRegionsResult.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let create_security_group config () =
      let result =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module CreateSecurityGroup)
               (Types.CreateSecurityGroupRequest.make
                  ~group_name:"aws-test-security_group"
                  ~description:"aws-test-security_group"
                  ())))
      in
      match result with
      | `Ok a -> Some a
      | `Error e ->
          print_endline (Aws.Error.format Errors_internal.to_string e);
          None

    let create_security_group_test config _ =
      let result = create_security_group config () in
      ("Creating security group succeeds"
      @?
      match result with
      | Some _ -> true
      | None -> false);
      let group_id =
        match result with
        | Some group -> group.group_id
        | None -> assert false
      in
      Unix.sleep 3;
      let result =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module DeleteSecurityGroup)
               (Types.DeleteSecurityGroupRequest.make ~group_id ())))
      in
      "Creating security group and then deleting it succeeds"
      @?
      match result with
      | `Ok _ -> true
      | `Error e ->
          print_endline (Aws.Error.format Errors_internal.to_string e);
          false

    let create_instance config () =
      let result =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module RunInstances)
               (Types.RunInstancesRequest.make
                  ~image_id:"ami-07fbdcfe29326c4fb"
                  ~min_count:1
                  ~max_count:1
                  ~instance_type:Types.InstanceType.T2_micro
                  ())))
      in
      let open Types.Reservation in
      match result with
      | `Ok reservation -> (
          match reservation.instances with
          | [] ->
              print_endline "No instances in reservation";
              None
          | x :: _ -> Some x)
      | `Error e ->
          print_endline (Aws.Error.format Errors_internal.to_string e);
          None

    let create config _ =
      let result = create_instance config () in
      ("Creating ec2 instance succeeds"
      @?
      match result with
      | Some _ -> true
      | None -> false);
      let instance_id =
        match result with
        | Some instance -> instance.Types.Instance.instance_id
        | None -> assert false
      in
      (* NOTE(dbp 2015-01-21): In seems that sometimes if you hit it quickly enough,
       * the api doesn't know about the instance and errors. This is obviously bad! *)
      Printf.printf "waiting for instance to be created";
      Unix.sleep 3;

      let result =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module TerminateInstances)
               (Types.TerminateInstancesRequest.make ~instance_ids:[ instance_id ] ())))
      in
      "Creating ec2 instances and then terminating it succeeds"
      @?
      match result with
      | `Ok _ -> true
      | `Error e ->
          print_endline (Aws.Error.format Errors_internal.to_string e);
          false

    let suite config =
      "Test EC2"
      >::: [ "Describe Regions" >:: describe_regions_json config
           ; "Create Instance" >:: create config
           ; "Find Security Group" >:: create_security_group_test config
           ]

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
