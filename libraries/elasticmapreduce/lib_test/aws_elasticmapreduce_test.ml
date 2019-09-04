open OUnit
open Aws_elasticmapreduce

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

  (* Tag for test EMR instances *)
  let test_tag = Types.Tag.make ~key:"client" ~value:"ocaml-aws" ()

  let create_describe_shutdown_test () =
    let res = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module RunJobFlow)
                               (Types.RunJobFlowInput.make
                                  ~name:"ocaml-aws test EMR"
                                  ~release_label:"emr-5.20.0"
                                  ~applications:([Types.Application.make ()])
                                  ~tags:(Types.TagList.make [test_tag] ())
                                  ~service_role:"EMR_DefaultRole"
                                  ~job_flow_role:"EMR_EC2_DefaultRole"
                                  (* TODO Invalid instance profile? What should this be? *)
                                  ~instances:(Types.JobFlowInstancesConfig.make
                                                ~instance_count:1
                                                ~master_instance_type:"t1.micro"
                                                ~slave_instance_type:"t1.micro"
                                                ())
                                  ())
              )) in
    "Create EMR"
    @? begin match res with
       | `Ok resp ->
          Printf.printf "%s\n" (Yojson.Basic.to_string (Types.RunJobFlowOutput.(to_json (of_json (to_json resp)))));
          true
       | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
       end;

    let instance_id = match res with
      | `Ok instance -> (match instance.job_flow_id with
                        | Some job_id -> job_id
                        | None -> assert false)
      | `Error err -> assert false in

    let terminate = Runtime.(un_m (run_request
                                     ~region:"us-east-1"
                                     (module TerminateJobFlows)
                                     (Types.TerminateJobFlowsInput.make ~job_flow_ids:[instance_id] ()))
                    ) in

    "Shutdown EMR"
    @? begin match terminate with
       | `Ok resp ->
          Printf.printf "Terminating EMR instance %s \n" instance_id;
          true
       | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
       end

  let test_cases =
    [ "Create EMR" >:: create_describe_shutdown_test ]

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
