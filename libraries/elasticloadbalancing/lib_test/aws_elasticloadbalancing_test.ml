open OUnit
open Aws_elasticloadbalancing

let from_opt = function
  | None -> assert false
  | Some(x) -> x

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
      region:string
      -> (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct


  let describe_load_balancers_json () =
    let res = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module DescribeLoadBalancers)
                               (Types.DescribeAccessPointsInput.make ())))
    in
    "DescribeLoadBalancers returns successfully, and to_json / from_json round trips result"
    @? begin match res with
      | `Ok resp ->
        Printf.printf "%s\n" (Yojson.Basic.to_string (Types.DescribeAccessPointsOutput.(to_json (of_json (to_json resp)))));
        true
      | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
    end

  let describe_load_balancers_error () =
    let res = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module DescribeLoadBalancers)
                               (Types.DescribeAccessPointsInput.make ~load_balancer_names:["non-existent-lb"] ())))
    in
    "DescribeLoadBalancers returns an LoadBalancerNotFound error."
    @? begin
      let open Aws.Error in
      match res with
      | `Error (HttpError (400, AwsError [Understood Errors_internal.LoadBalancerNotFound, _])) ->
        true
      | `Error (HttpError (_, AwsError [Unknown code, message])) ->
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
        Printf.printf "Other error: %s\n%!" (Aws.Error.format Errors_internal.to_string msg);
        false
    end


  let create_load_balancers () =
    let kill_lb () = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module DeleteLoadBalancer)
                               (Types.DeleteAccessPointInput.make
                                  ~load_balancer_name:"foobar"
                                  ()
                               ))) in
    let res = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module CreateLoadBalancer)
                               (Types.CreateAccessPointInput.make
                                  ~load_balancer_name:"foobar"
                                  ~availability_zones:(Types.AvailabilityZones.make
                                                         ["us-east-1b";"us-east-1c"
                                                         ;"us-east-1d";"us-east-1e"]
                                                         ())
                                  ~tags:[Types.Tag.make ~key:"foo" ()]
                                  ~listeners:[Types.Listener.make ~protocol:"HTTP" ~load_balancer_port:80 ~instance_port:80 ()]
                                  ()
                               )))
    in
    "CreateLoadBalancer returns successfully"
    @? begin match res with
      | `Ok resp -> true
      | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
    end;
    let res = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module DescribeTags)
                               (Types.DescribeTagsInput.make
                                  ~load_balancer_names:["foobar"]
                                  ()
                               )))
    in
    "DescribeTags returns the tag 'foo'"
    @? begin match res with
      | `Ok res -> begin match res.Types.DescribeTagsOutput.tag_descriptions with
          | (_::_) -> true
          | [] -> begin Printf.printf "No tags returned\n"; ignore (kill_lb ()); false end
        end
      | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); ignore (kill_lb ()); false end
    end;
    let res = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module ConfigureHealthCheck)
                               (Types.ConfigureHealthCheckInput.make
                                  ~load_balancer_name:"foobar"
                                  ~health_check:(Types.HealthCheck.make
                                                   ~target:"TCP:80"
                                                   ~interval:5
                                                   ~timeout:3
                                                   ~healthy_threshold:2
                                                   ~unhealthy_threshold:2
                                                   ())
                                  ())))
    in

    "Can ConfigureHealthCheck"
    @? begin match res with
      | `Ok resp -> true
      | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
    end;
    let res = kill_lb ()
    in
    "DeleteLoadBalancer returns successfully"
    @? begin match res with
      | `Ok resp -> true
      | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
    end



  let test_cases = ["Describe Load Balancers" >:: describe_load_balancers_json;
                    "Describe Load Balancers Error" >:: describe_load_balancers_error;
                    "Create Load Balancer and Tags" >:: create_load_balancers;
                   ]

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
    let suite = "Lwt Tests" >::: test_cases in
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
      exit 1

end
