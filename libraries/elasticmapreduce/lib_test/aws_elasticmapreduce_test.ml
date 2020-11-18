open OUnit
open Aws_elasticmapreduce

module TestSuite (Runtime : sig
  type 'a m

  val run_request :
       region:string
    -> (module Aws.Call
          with type input = 'input
           and type output = 'output
           and type error = 'error)
    -> 'input
    -> [ `Ok of 'output | `Error of 'error Aws.Error.t ] m

  val un_m : 'a m -> 'a
end) =
struct
  (* Tag for test EMR instances *)
  let test_tag = Types.Tag.make ~key:"client" ~value:"ocaml-aws" ()

  let region = "ap-southeast-2"

  let steps =
    Types.StepConfig.make
      ~name:"Example Streaming Step"
      ~action_on_failure:Types.ActionOnFailure.CANCEL_AND_WAIT
      ~hadoop_jar_step:
        (Types.HadoopJarStepConfig.make
           ~jar:"/home/hadoop/contrib/streaming/hadoop-streaming.jar"
           ~args:
             (Types.XmlStringList.make
                [ "-input"
                ; "s3://elasticmapreduce/samples/wordcount/input"
                ; "-output"
                ; "s3://examples-bucket/example-output"
                ; "-mapper"
                ; "s3://elasticmapreduce/samples/wordcount/wordSplitter.py"
                ; "-reducer"
                ; "aggregate"
                ]
                ())
           ())
      ()

  (* TODO The response from Create
   * {
   *  "ClusterId": "j-3RQ0GU8ICBSW2",
   *  "ClusterArn": "arn:aws:elasticmapreduce:ap-southeast-2:779241156015:cluster/j-3RQ0GU8ICBSW2"
   * } *)

  let list_emr_test () =
    let res =
      Runtime.(
        un_m
          (run_request
             ~region
             (module ListClusters)
             (Types.(
                ListClustersInput.make
                  ~cluster_states:(ClusterStateList.make ClusterState.[ TERMINATED ] ()))
                ())))
    in
    "List terminated EMR Clusters"
    @?
    match res with
    | `Ok resp ->
        Printf.printf
          "%s\n"
          (Yojson.Basic.to_string
             Types.ListClustersOutput.(to_json (of_json (to_json resp))));
        true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false

  let create_describe_shutdown_test () =
    let res =
      Runtime.(
        un_m
          (run_request
             ~region
             (module RunJobFlow)
             (Types.RunJobFlowInput.make
                ~name:"ocaml-aws test EMR"
                ~instances:
                  (Types.JobFlowInstancesConfig.make
                     ~instance_count:1
                     ~master_instance_type:"m1.medium"
                     ~slave_instance_type:"m1.medium"
                     ())
                ~release_label:"emr-5.31.0"
                ~service_role:"EMR_DefaultRole"
                ~steps:(Types.StepConfigList.make [ steps ] ())
                ~job_flow_role:"EMR_EC2_DefaultRole"
                ~scale_down_behavior:Types.ScaleDownBehavior.TERMINATE_AT_TASK_COMPLETION
                ())))
    in
    ("Create EMR"
    @?
    match res with
    | `Ok resp ->
        Printf.printf
          "%s\n"
          (Yojson.Basic.to_string
             Types.RunJobFlowOutput.(to_json (of_json (to_json resp))));
        true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false);

    let instance_id =
      match res with
      | `Ok instance -> (
          match instance.job_flow_id with
          | Some job_id -> job_id
          | None -> assert false)
      | `Error err -> assert false
    in

    let terminate =
      Runtime.(
        un_m
          (run_request
             ~region
             (module TerminateJobFlows)
             (Types.TerminateJobFlowsInput.make ~job_flow_ids:[ instance_id ] ())))
    in

    "Shutdown EMR"
    @?
    match terminate with
    | `Ok resp ->
        Printf.printf "Terminating EMR instance %s \n" instance_id;
        true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false

  let test_cases =
    [ (* "Create EMR" >:: create_describe_shutdown_test *)
      (* , *)
      "List EMR" >:: list_emr_test
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
