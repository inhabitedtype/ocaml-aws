open Aws.BaseTypes

type t =
  { scheduled_action_name : String.t
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; recurrence : String.t option
  ; min_size : Integer.t option
  ; max_size : Integer.t option
  ; desired_capacity : Integer.t option
  ; time_zone : String.t option
  }

let make
    ~scheduled_action_name
    ?start_time
    ?end_time
    ?recurrence
    ?min_size
    ?max_size
    ?desired_capacity
    ?time_zone
    () =
  { scheduled_action_name
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
    { scheduled_action_name =
        Aws.Xml.required
          "ScheduledActionName"
          (Aws.Util.option_bind (Aws.Xml.member "ScheduledActionName" xml) String.parse)
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
       ; Some
           (Aws.Query.Pair ("ScheduledActionName", String.to_query v.scheduled_action_name))
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
       ; Some ("ScheduledActionName", String.to_json v.scheduled_action_name)
       ])

let of_json j =
  { scheduled_action_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledActionName"))
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; recurrence = Aws.Util.option_map (Aws.Json.lookup j "Recurrence") String.of_json
  ; min_size = Aws.Util.option_map (Aws.Json.lookup j "MinSize") Integer.of_json
  ; max_size = Aws.Util.option_map (Aws.Json.lookup j "MaxSize") Integer.of_json
  ; desired_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "DesiredCapacity") Integer.of_json
  ; time_zone = Aws.Util.option_map (Aws.Json.lookup j "TimeZone") String.of_json
  }
