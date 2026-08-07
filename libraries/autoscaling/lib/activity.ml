open Aws.BaseTypes

type t =
  { activity_id : String.t
  ; auto_scaling_group_name : String.t
  ; description : String.t option
  ; cause : String.t
  ; start_time : DateTime.t
  ; end_time : DateTime.t option
  ; status_code : ScalingActivityStatusCode.t
  ; status_message : String.t option
  ; progress : Integer.t option
  ; details : String.t option
  ; auto_scaling_group_state : String.t option
  ; auto_scaling_group_a_r_n : String.t option
  }

let make
    ~activity_id
    ~auto_scaling_group_name
    ?description
    ~cause
    ~start_time
    ?end_time
    ~status_code
    ?status_message
    ?progress
    ?details
    ?auto_scaling_group_state
    ?auto_scaling_group_a_r_n
    () =
  { activity_id
  ; auto_scaling_group_name
  ; description
  ; cause
  ; start_time
  ; end_time
  ; status_code
  ; status_message
  ; progress
  ; details
  ; auto_scaling_group_state
  ; auto_scaling_group_a_r_n
  }

let parse xml =
  Some
    { activity_id =
        Aws.Xml.required
          "ActivityId"
          (Aws.Util.option_bind (Aws.Xml.member "ActivityId" xml) String.parse)
    ; auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; cause =
        Aws.Xml.required
          "Cause"
          (Aws.Util.option_bind (Aws.Xml.member "Cause" xml) String.parse)
    ; start_time =
        Aws.Xml.required
          "StartTime"
          (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; status_code =
        Aws.Xml.required
          "StatusCode"
          (Aws.Util.option_bind
             (Aws.Xml.member "StatusCode" xml)
             ScalingActivityStatusCode.parse)
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "StatusMessage" xml) String.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "Progress" xml) Integer.parse
    ; details = Aws.Util.option_bind (Aws.Xml.member "Details" xml) String.parse
    ; auto_scaling_group_state =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupState" xml) String.parse
    ; auto_scaling_group_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_scaling_group_a_r_n (fun f ->
             Aws.Query.Pair ("AutoScalingGroupARN", String.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_state (fun f ->
             Aws.Query.Pair ("AutoScalingGroupState", String.to_query f))
       ; Aws.Util.option_map v.details (fun f ->
             Aws.Query.Pair ("Details", String.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", Integer.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Some
           (Aws.Query.Pair ("StatusCode", ScalingActivityStatusCode.to_query v.status_code))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ; Some (Aws.Query.Pair ("Cause", String.to_query v.cause))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ; Some (Aws.Query.Pair ("ActivityId", String.to_query v.activity_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_scaling_group_a_r_n (fun f ->
             "AutoScalingGroupARN", String.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_state (fun f ->
             "AutoScalingGroupState", String.to_json f)
       ; Aws.Util.option_map v.details (fun f -> "Details", String.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "Progress", Integer.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "StatusMessage", String.to_json f)
       ; Some ("StatusCode", ScalingActivityStatusCode.to_json v.status_code)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Some ("StartTime", DateTime.to_json v.start_time)
       ; Some ("Cause", String.to_json v.cause)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ; Some ("ActivityId", String.to_json v.activity_id)
       ])

let of_json j =
  { activity_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ActivityId"))
  ; auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; cause = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cause"))
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; status_code =
      ScalingActivityStatusCode.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StatusCode"))
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "StatusMessage") String.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "Progress") Integer.of_json
  ; details = Aws.Util.option_map (Aws.Json.lookup j "Details") String.of_json
  ; auto_scaling_group_state =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupState") String.of_json
  ; auto_scaling_group_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupARN") String.of_json
  }
