open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; dry_run : Boolean.t option
  ; instance_count : Integer.t option
  ; launch_specification : ScheduledInstancesLaunchSpecification.t
  ; scheduled_instance_id : String.t
  }

let make
    ?client_token
    ?dry_run
    ?instance_count
    ~launch_specification
    ~scheduled_instance_id
    () =
  { client_token; dry_run; instance_count; launch_specification; scheduled_instance_id }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse
    ; launch_specification =
        Aws.Xml.required
          "LaunchSpecification"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchSpecification" xml)
             ScheduledInstancesLaunchSpecification.parse)
    ; scheduled_instance_id =
        Aws.Xml.required
          "ScheduledInstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "ScheduledInstanceId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ScheduledInstanceId", String.to_query v.scheduled_instance_id))
       ; Some
           (Aws.Query.Pair
              ( "LaunchSpecification"
              , ScheduledInstancesLaunchSpecification.to_query v.launch_specification ))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ScheduledInstanceId", String.to_json v.scheduled_instance_id)
       ; Some
           ( "LaunchSpecification"
           , ScheduledInstancesLaunchSpecification.to_json v.launch_specification )
       ; Aws.Util.option_map v.instance_count (fun f ->
             "InstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceCount") Integer.of_json
  ; launch_specification =
      ScheduledInstancesLaunchSpecification.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchSpecification"))
  ; scheduled_instance_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledInstanceId"))
  }
