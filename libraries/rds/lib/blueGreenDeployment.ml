open Aws.BaseTypes

type t =
  { blue_green_deployment_identifier : String.t option
  ; blue_green_deployment_name : String.t option
  ; source : String.t option
  ; target : String.t option
  ; switchover_details : SwitchoverDetailList.t
  ; tasks : BlueGreenDeploymentTaskList.t
  ; status : String.t option
  ; status_details : String.t option
  ; create_time : DateTime.t option
  ; delete_time : DateTime.t option
  ; tag_list : TagList.t
  }

let make
    ?blue_green_deployment_identifier
    ?blue_green_deployment_name
    ?source
    ?target
    ?(switchover_details = [])
    ?(tasks = [])
    ?status
    ?status_details
    ?create_time
    ?delete_time
    ?(tag_list = [])
    () =
  { blue_green_deployment_identifier
  ; blue_green_deployment_name
  ; source
  ; target
  ; switchover_details
  ; tasks
  ; status
  ; status_details
  ; create_time
  ; delete_time
  ; tag_list
  }

let parse xml =
  Some
    { blue_green_deployment_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "BlueGreenDeploymentIdentifier" xml)
          String.parse
    ; blue_green_deployment_name =
        Aws.Util.option_bind (Aws.Xml.member "BlueGreenDeploymentName" xml) String.parse
    ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
    ; target = Aws.Util.option_bind (Aws.Xml.member "Target" xml) String.parse
    ; switchover_details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SwitchoverDetails" xml)
             SwitchoverDetailList.parse)
    ; tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Tasks" xml)
             BlueGreenDeploymentTaskList.parse)
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; status_details =
        Aws.Util.option_bind (Aws.Xml.member "StatusDetails" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "CreateTime" xml) DateTime.parse
    ; delete_time = Aws.Util.option_bind (Aws.Xml.member "DeleteTime" xml) DateTime.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.delete_time (fun f ->
             Aws.Query.Pair ("DeleteTime", DateTime.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status_details (fun f ->
             Aws.Query.Pair ("StatusDetails", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Tasks.member", BlueGreenDeploymentTaskList.to_query v.tasks))
       ; Some
           (Aws.Query.Pair
              ( "SwitchoverDetails.member"
              , SwitchoverDetailList.to_query v.switchover_details ))
       ; Aws.Util.option_map v.target (fun f ->
             Aws.Query.Pair ("Target", String.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", String.to_query f))
       ; Aws.Util.option_map v.blue_green_deployment_name (fun f ->
             Aws.Query.Pair ("BlueGreenDeploymentName", String.to_query f))
       ; Aws.Util.option_map v.blue_green_deployment_identifier (fun f ->
             Aws.Query.Pair ("BlueGreenDeploymentIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.delete_time (fun f -> "DeleteTime", DateTime.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "CreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status_details (fun f -> "StatusDetails", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Some ("Tasks", BlueGreenDeploymentTaskList.to_json v.tasks)
       ; Some ("SwitchoverDetails", SwitchoverDetailList.to_json v.switchover_details)
       ; Aws.Util.option_map v.target (fun f -> "Target", String.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
       ; Aws.Util.option_map v.blue_green_deployment_name (fun f ->
             "BlueGreenDeploymentName", String.to_json f)
       ; Aws.Util.option_map v.blue_green_deployment_identifier (fun f ->
             "BlueGreenDeploymentIdentifier", String.to_json f)
       ])

let of_json j =
  { blue_green_deployment_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "BlueGreenDeploymentIdentifier")
        String.of_json
  ; blue_green_deployment_name =
      Aws.Util.option_map (Aws.Json.lookup j "BlueGreenDeploymentName") String.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
  ; target = Aws.Util.option_map (Aws.Json.lookup j "Target") String.of_json
  ; switchover_details =
      SwitchoverDetailList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SwitchoverDetails"))
  ; tasks =
      BlueGreenDeploymentTaskList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Tasks"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; status_details =
      Aws.Util.option_map (Aws.Json.lookup j "StatusDetails") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "CreateTime") DateTime.of_json
  ; delete_time = Aws.Util.option_map (Aws.Json.lookup j "DeleteTime") DateTime.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  }
