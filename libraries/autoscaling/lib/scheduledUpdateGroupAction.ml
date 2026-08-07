open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t option
  ; scheduled_action_name : String.t option
  ; scheduled_action_a_r_n : String.t option
  ; time : DateTime.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; recurrence : String.t option
  ; min_size : Integer.t option
  ; max_size : Integer.t option
  ; desired_capacity : Integer.t option
  ; time_zone : String.t option
  }

let make
    ?auto_scaling_group_name
    ?scheduled_action_name
    ?scheduled_action_a_r_n
    ?time
    ?start_time
    ?end_time
    ?recurrence
    ?min_size
    ?max_size
    ?desired_capacity
    ?time_zone
    () =
  { auto_scaling_group_name
  ; scheduled_action_name
  ; scheduled_action_a_r_n
  ; time
  ; start_time
  ; end_time
  ; recurrence
  ; min_size
  ; max_size
  ; desired_capacity
  ; time_zone
  }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; scheduled_action_name =
        Aws.Util.option_bind (Aws.Xml.member "ScheduledActionName" xml) String.parse
    ; scheduled_action_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "ScheduledActionARN" xml) String.parse
    ; time = Aws.Util.option_bind (Aws.Xml.member "Time" xml) DateTime.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; recurrence = Aws.Util.option_bind (Aws.Xml.member "Recurrence" xml) String.parse
    ; min_size = Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse
    ; max_size = Aws.Util.option_bind (Aws.Xml.member "MaxSize" xml) Integer.parse
    ; desired_capacity =
        Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse
    ; time_zone = Aws.Util.option_bind (Aws.Xml.member "TimeZone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.time_zone (fun f ->
             Aws.Query.Pair ("TimeZone", String.to_query f))
       ; Aws.Util.option_map v.desired_capacity (fun f ->
             Aws.Query.Pair ("DesiredCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.max_size (fun f ->
             Aws.Query.Pair ("MaxSize", Integer.to_query f))
       ; Aws.Util.option_map v.min_size (fun f ->
             Aws.Query.Pair ("MinSize", Integer.to_query f))
       ; Aws.Util.option_map v.recurrence (fun f ->
             Aws.Query.Pair ("Recurrence", String.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.time (fun f ->
             Aws.Query.Pair ("Time", DateTime.to_query f))
       ; Aws.Util.option_map v.scheduled_action_a_r_n (fun f ->
             Aws.Query.Pair ("ScheduledActionARN", String.to_query f))
       ; Aws.Util.option_map v.scheduled_action_name (fun f ->
             Aws.Query.Pair ("ScheduledActionName", String.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.time_zone (fun f -> "TimeZone", String.to_json f)
       ; Aws.Util.option_map v.desired_capacity (fun f ->
             "DesiredCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.max_size (fun f -> "MaxSize", Integer.to_json f)
       ; Aws.Util.option_map v.min_size (fun f -> "MinSize", Integer.to_json f)
       ; Aws.Util.option_map v.recurrence (fun f -> "Recurrence", String.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.time (fun f -> "Time", DateTime.to_json f)
       ; Aws.Util.option_map v.scheduled_action_a_r_n (fun f ->
             "ScheduledActionARN", String.to_json f)
       ; Aws.Util.option_map v.scheduled_action_name (fun f ->
             "ScheduledActionName", String.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ])

let of_json j =
  { auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; scheduled_action_name =
      Aws.Util.option_map (Aws.Json.lookup j "ScheduledActionName") String.of_json
  ; scheduled_action_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "ScheduledActionARN") String.of_json
  ; time = Aws.Util.option_map (Aws.Json.lookup j "Time") DateTime.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; recurrence = Aws.Util.option_map (Aws.Json.lookup j "Recurrence") String.of_json
  ; min_size = Aws.Util.option_map (Aws.Json.lookup j "MinSize") Integer.of_json
  ; max_size = Aws.Util.option_map (Aws.Json.lookup j "MaxSize") Integer.of_json
  ; desired_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacity") Integer.of_json
  ; time_zone = Aws.Util.option_map (Aws.Json.lookup j "TimeZone") String.of_json
  }
