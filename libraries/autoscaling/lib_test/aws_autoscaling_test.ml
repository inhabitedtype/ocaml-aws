open OUnit2
open Aws_autoscaling

let from_opt = function
  | None -> OUnit.assert_failure "from_opt is None!"
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
    let create_autoscaling ~instance_id ~auto_scaling_group_name config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module CreateAutoScalingGroup)
             (Types.CreateAutoScalingGroupType.make
                ~auto_scaling_group_name
                ~min_size:1
                ~max_size:1
                ~instance_id
                ())))

    let delete_autoscaling ~auto_scaling_group_name:_ config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module DeleteAutoScalingGroup)
             (Types.DeleteAutoScalingGroupType.make
                ~auto_scaling_group_name:"aws-test-autoscaling_group"
                ())))

    let create_instance config () =
      let open Aws_ec2 in
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

    let create_autoscaling_group_test config _ =
      let auto_scaling_group_name = "aws-test-autoscaling_group" in

      let instance_opt = create_instance config () in
      (* NOTE(tsmc 2020-11-21): Waiting on an instance to be created varies depending on the instance and
         the cloud conditions. The following timeout should really be a polling operation. *)
      Printf.printf "waiting for instance to be created\n";
      Unix.sleep 60;

      let instance_id = from_opt instance_opt in
      let result =
        create_autoscaling
          ~instance_id:instance_id.Aws_ec2.Types.Instance.instance_id
          ~auto_scaling_group_name
          config
          ()
      in
      ("Creating autoscaling group succeeds"
      @?
      match result with
      | `Ok _ -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let result = delete_autoscaling ~auto_scaling_group_name config () in
      "Delete autoscaling group succeeds"
      @?
      match result with
      | `Ok _ -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let suite config =
      "Test Autoscaling"
      >::: [ "Create Autoscaling group" >:: create_autoscaling_group_test config ]

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
          Printf.printf
            "Skipping running tests. Environment variables AWS_ACCESS_KEY_ID, \
             AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION not available.";
          exit 0
  end
