open OUnit
open Aws_ec2

let from_opt = function
  | None -> assert false
  | Some x -> x

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
    let describe_regions_json () =
      let res =
        Runtime.(
          un_m
            (run_request (module DescribeRegions) (Types.DescribeRegionsRequest.make ())))
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

    let create_security_group () =
      let result =
        Runtime.(
          un_m
            (run_request
               (module CreateSecurityGroup)
               (Types.CreateSecurityGroupRequest.make
                  ~group_name:"aws-test-security_group"
                  ~description:"aws-test-security_group"
                  ())))
      in
      (* let open Types.SecurityGroup in *)
      match result with
      | `Ok a -> Some a
      | `Error e ->
          print_endline (Aws.Error.format Errors_internal.to_string e);
          None

    let create_security_group_test () =
      let result = create_security_group () in
      ("Creating security group succeeds"
      @?
      match result with
      | Some group -> true
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

    let create_instance () =
      let result =
        Runtime.(
          un_m
            (run_request
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
          | x :: xs -> Some x)
      | `Error e ->
          print_endline (Aws.Error.format Errors_internal.to_string e);
          None

    let create () =
      let result = create_instance () in
      ("Creating ec2 instance succeeds"
      @?
      match result with
      | Some instance -> true
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

    let test_cases =
      [ "Describe Regions" >:: describe_regions_json
      ; "Create Instance" >:: create
      ; "Find Security Group" >:: create_security_group_test
      ]

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
