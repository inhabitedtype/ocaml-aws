open Aws.BaseTypes

type t =
  { instance_refresh_id : String.t option
  ; auto_scaling_group_name : String.t option
  ; status : InstanceRefreshStatus.t option
  ; status_reason : String.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; percentage_complete : Integer.t option
  ; instances_to_update : Integer.t option
  ; progress_details : InstanceRefreshProgressDetails.t option
  ; preferences : RefreshPreferences.t option
  ; desired_configuration : DesiredConfiguration.t option
  ; rollback_details : RollbackDetails.t option
  ; strategy : RefreshStrategy.t option
  }

let make
    ?instance_refresh_id
    ?auto_scaling_group_name
    ?status
    ?status_reason
    ?start_time
    ?end_time
    ?percentage_complete
    ?instances_to_update
    ?progress_details
    ?preferences
    ?desired_configuration
    ?rollback_details
    ?strategy
    () =
  { instance_refresh_id
  ; auto_scaling_group_name
  ; status
  ; status_reason
  ; start_time
  ; end_time
  ; percentage_complete
  ; instances_to_update
  ; progress_details
  ; preferences
  ; desired_configuration
  ; rollback_details
  ; strategy
  }

let parse xml =
  Some
    { instance_refresh_id =
        Aws.Util.option_bind (Aws.Xml.member "InstanceRefreshId" xml) String.parse
    ; auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) InstanceRefreshStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; percentage_complete =
        Aws.Util.option_bind (Aws.Xml.member "PercentageComplete" xml) Integer.parse
    ; instances_to_update =
        Aws.Util.option_bind (Aws.Xml.member "InstancesToUpdate" xml) Integer.parse
    ; progress_details =
        Aws.Util.option_bind
          (Aws.Xml.member "ProgressDetails" xml)
          InstanceRefreshProgressDetails.parse
    ; preferences =
        Aws.Util.option_bind (Aws.Xml.member "Preferences" xml) RefreshPreferences.parse
    ; desired_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "DesiredConfiguration" xml)
          DesiredConfiguration.parse
    ; rollback_details =
        Aws.Util.option_bind (Aws.Xml.member "RollbackDetails" xml) RollbackDetails.parse
    ; strategy =
        Aws.Util.option_bind (Aws.Xml.member "Strategy" xml) RefreshStrategy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.strategy (fun f ->
             Aws.Query.Pair ("Strategy", RefreshStrategy.to_query f))
       ; Aws.Util.option_map v.rollback_details (fun f ->
             Aws.Query.Pair ("RollbackDetails", RollbackDetails.to_query f))
       ; Aws.Util.option_map v.desired_configuration (fun f ->
             Aws.Query.Pair ("DesiredConfiguration", DesiredConfiguration.to_query f))
       ; Aws.Util.option_map v.preferences (fun f ->
             Aws.Query.Pair ("Preferences", RefreshPreferences.to_query f))
       ; Aws.Util.option_map v.progress_details (fun f ->
             Aws.Query.Pair ("ProgressDetails", InstanceRefreshProgressDetails.to_query f))
       ; Aws.Util.option_map v.instances_to_update (fun f ->
             Aws.Query.Pair ("InstancesToUpdate", Integer.to_query f))
       ; Aws.Util.option_map v.percentage_complete (fun f ->
             Aws.Query.Pair ("PercentageComplete", Integer.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", InstanceRefreshStatus.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ; Aws.Util.option_map v.instance_refresh_id (fun f ->
             Aws.Query.Pair ("InstanceRefreshId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.strategy (fun f -> "Strategy", RefreshStrategy.to_json f)
       ; Aws.Util.option_map v.rollback_details (fun f ->
             "RollbackDetails", RollbackDetails.to_json f)
       ; Aws.Util.option_map v.desired_configuration (fun f ->
             "DesiredConfiguration", DesiredConfiguration.to_json f)
       ; Aws.Util.option_map v.preferences (fun f ->
             "Preferences", RefreshPreferences.to_json f)
       ; Aws.Util.option_map v.progress_details (fun f ->
             "ProgressDetails", InstanceRefreshProgressDetails.to_json f)
       ; Aws.Util.option_map v.instances_to_update (fun f ->
             "InstancesToUpdate", Integer.to_json f)
       ; Aws.Util.option_map v.percentage_complete (fun f ->
             "PercentageComplete", Integer.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", InstanceRefreshStatus.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ; Aws.Util.option_map v.instance_refresh_id (fun f ->
             "InstanceRefreshId", String.to_json f)
       ])

let of_json j =
  { instance_refresh_id =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceRefreshId") String.of_json
  ; auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") InstanceRefreshStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; percentage_complete =
      Aws.Util.option_map (Aws.Json.lookup j "PercentageComplete") Integer.of_json
  ; instances_to_update =
      Aws.Util.option_map (Aws.Json.lookup j "InstancesToUpdate") Integer.of_json
  ; progress_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "ProgressDetails")
        InstanceRefreshProgressDetails.of_json
  ; preferences =
      Aws.Util.option_map (Aws.Json.lookup j "Preferences") RefreshPreferences.of_json
  ; desired_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "DesiredConfiguration")
        DesiredConfiguration.of_json
  ; rollback_details =
      Aws.Util.option_map (Aws.Json.lookup j "RollbackDetails") RollbackDetails.of_json
  ; strategy = Aws.Util.option_map (Aws.Json.lookup j "Strategy") RefreshStrategy.of_json
  }
