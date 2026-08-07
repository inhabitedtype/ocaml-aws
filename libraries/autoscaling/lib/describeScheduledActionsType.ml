open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t option
  ; scheduled_action_names : ScheduledActionNames.t
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; next_token : String.t option
  ; max_records : Integer.t option
  }

let make
    ?auto_scaling_group_name
    ?(scheduled_action_names = [])
    ?start_time
    ?end_time
    ?next_token
    ?max_records
    () =
  { auto_scaling_group_name
  ; scheduled_action_names
  ; start_time
  ; end_time
  ; next_token
  ; max_records
  }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; scheduled_action_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ScheduledActionNames" xml)
             ScheduledActionNames.parse)
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ScheduledActionNames.member"
              , ScheduledActionNames.to_query v.scheduled_action_names ))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Some
           ("ScheduledActionNames", ScheduledActionNames.to_json v.scheduled_action_names)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ])

let of_json j =
  { auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; scheduled_action_names =
      ScheduledActionNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledActionNames"))
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  }
