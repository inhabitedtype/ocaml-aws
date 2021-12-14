open OUnit2
open Aws_elasticloadbalancing

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
    let describe_load_balancers_json config _ =
      let res =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module DescribeLoadBalancers)
               (Types.DescribeAccessPointsInput.make ())))
      in
      "DescribeLoadBalancers returns successfully, and to_json / from_json round trips \
       result"
      @?
      match res with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.to_string
               Types.DescribeAccessPointsOutput.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let describe_load_balancers_error config _ =
      let res =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module DescribeLoadBalancers)
               (Types.DescribeAccessPointsInput.make
                  ~load_balancer_names:[ "non-existent-lb" ]
                  ())))
      in
      "DescribeLoadBalancers returns an LoadBalancerNotFound error."
      @?
      let open Aws.Error in
      match res with
      | `Error
          (HttpError
            (400, AwsError [ (Understood Errors_internal.LoadBalancerNotFound, _) ])) ->
          true
      | `Error (HttpError (_, AwsError [ (Unknown code, message) ])) ->
          Printf.printf "Couldn't understand error: %s -- %s\n%!" code message;
          false
      | `Ok _ ->
          Printf.printf "Got Ok response, which shouldn't have happened.\n%!";
          false
      | `Error (HttpError (_, BadResponse { body; message })) ->
          Printf.printf "Got bad response: %s ---- %s ---- bleh\n%!" message body;
          false
      | `Error (TransportError msg) ->
          Printf.printf "Got Transport error: %s\n%!" msg;
          false
      | `Error msg ->
          Printf.printf
            "Other error: %s\n%!"
            (Aws.Error.format Errors_internal.to_string msg);
          false

    let create_load_balancers config _ =
      let kill_lb () =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module DeleteLoadBalancer)
               (Types.DeleteAccessPointInput.make ~load_balancer_name:"foobar" ())))
      in
      let res =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module CreateLoadBalancer)
               (Types.CreateAccessPointInput.make
                  ~load_balancer_name:"foobar"
                  ~availability_zones:
                    (Types.AvailabilityZones.make
                       [ "ap-southeast-2a"; "ap-southeast-2b" ]
                       ())
                  ~tags:[ Types.Tag.make ~key:"foo" () ]
                  ~listeners:
                    [ Types.Listener.make
                        ~protocol:"HTTP"
                        ~load_balancer_port:80
                        ~instance_port:80
                        ()
                    ]
                  ())))
      in
      ("CreateLoadBalancer returns successfully"
      @?
      match res with
      | `Ok _ -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let res =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module DescribeTags)
               (Types.DescribeTagsInput.make ~load_balancer_names:[ "foobar" ] ())))
      in
      ("DescribeTags returns the tag 'foo'"
      @?
      match res with
      | `Ok res -> (
          match res.Types.DescribeTagsOutput.tag_descriptions with
          | _ :: _ -> true
          | [] ->
              Printf.printf "No tags returned\n";
              ignore (kill_lb ());
              false)
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          ignore (kill_lb ());
          false);
      let res =
        Runtime.(
          un_m
            (run_request
               ~region:config.region
               ~access_key:config.access_key
               ~secret_key:config.secret_key
               (module ConfigureHealthCheck)
               (Types.ConfigureHealthCheckInput.make
                  ~load_balancer_name:"foobar"
                  ~health_check:
                    (Types.HealthCheck.make
                       ~target:"TCP:80"
                       ~interval:5
                       ~timeout:3
                       ~healthy_threshold:2
                       ~unhealthy_threshold:2
                       ())
                  ())))
      in

      ("Can ConfigureHealthCheck"
      @?
      match res with
      | `Ok _ -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let res = kill_lb () in
      "DeleteLoadBalancer returns successfully"
      @?
      match res with
      | `Ok _ -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let suite config =
      "Test ElasticLoadBalancing"
      >::: [ "Describe Load Balancers" >:: describe_load_balancers_json config
           ; "Describe Load Balancers Error" >:: describe_load_balancers_error config
           ; "Create Load Balancer and Tags" >:: create_load_balancers config
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
