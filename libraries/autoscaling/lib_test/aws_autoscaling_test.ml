open OUnit
open Aws_autoscaling

let from_opt = function
  | None -> OUnit.assert_failure "from_opt is None!"
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
    let create_autoscaling ~instance_id ~auto_scaling_group_name () =
      Runtime.(
        un_m
          (run_request
             (module CreateAutoScalingGroup)
             (Types.CreateAutoScalingGroupType.make
                ~auto_scaling_group_name
                ~min_size:1
                ~max_size:1
                ~instance_id
                ())))

    let delete_autoscaling ~auto_scaling_group_name () =
      Runtime.(
        un_m
          (run_request
             (module DeleteAutoScalingGroup)
             (Types.DeleteAutoScalingGroupType.make
                ~auto_scaling_group_name:"aws-test-autoscaling_group"
                ())))

    let create_instance () =
      let open Aws_ec2 in
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

    let create_autoscaling_group_test () =
      let auto_scaling_group_name = "aws-test-autoscaling_group" in

      let instance_opt = create_instance () in
      (* NOTE(tsmc 2020-11-21): Waiting on an instance to be created varies depending on the instance and
         the cloud conditions. The following timeout should really be a polling operation. *)
      Printf.printf "waiting for instance to be created\n";
      Unix.sleep 60;

      let instance_id = from_opt instance_opt in
      let result =
        create_autoscaling
          ~instance_id:instance_id.Aws_ec2.Types.Instance.instance_id
          ~auto_scaling_group_name
          ()
      in
      ("Creating autoscaling group succeeds"
      @?
      match result with
      | `Ok instance -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let result = delete_autoscaling ~auto_scaling_group_name () in
      "Delete autoscaling group succeeds"
      @?
      match result with
      | `Ok instance -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let test_cases = [ "Create Autoscaling group" >:: create_autoscaling_group_test ]

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
