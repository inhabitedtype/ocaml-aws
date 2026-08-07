open Aws.BaseTypes

type t =
  { trail_a_r_n : String.t option
  ; event_source : String.t option
  ; event_name : String.t option
  ; insight_type : InsightType.t option
  ; error_code : String.t option
  ; timestamps : Timestamps.t
  ; values : InsightsMetricValues.t
  ; next_token : String.t option
  }

let make
    ?trail_a_r_n
    ?event_source
    ?event_name
    ?insight_type
    ?error_code
    ?(timestamps = [])
    ?(values = [])
    ?next_token
    () =
  { trail_a_r_n
  ; event_source
  ; event_name
  ; insight_type
  ; error_code
  ; timestamps
  ; values
  ; next_token
  }

let parse xml =
  Some
    { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
    ; event_source = Aws.Util.option_bind (Aws.Xml.member "EventSource" xml) String.parse
    ; event_name = Aws.Util.option_bind (Aws.Xml.member "EventName" xml) String.parse
    ; insight_type =
        Aws.Util.option_bind (Aws.Xml.member "InsightType" xml) InsightType.parse
    ; error_code = Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) String.parse
    ; timestamps =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Timestamps" xml) Timestamps.parse)
    ; values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Values" xml) InsightsMetricValues.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Values.member", InsightsMetricValues.to_query v.values))
       ; Some (Aws.Query.Pair ("Timestamps.member", Timestamps.to_query v.timestamps))
       ; Aws.Util.option_map v.error_code (fun f ->
             Aws.Query.Pair ("ErrorCode", String.to_query f))
       ; Aws.Util.option_map v.insight_type (fun f ->
             Aws.Query.Pair ("InsightType", InsightType.to_query f))
       ; Aws.Util.option_map v.event_name (fun f ->
             Aws.Query.Pair ("EventName", String.to_query f))
       ; Aws.Util.option_map v.event_source (fun f ->
             Aws.Query.Pair ("EventSource", String.to_query f))
       ; Aws.Util.option_map v.trail_a_r_n (fun f ->
             Aws.Query.Pair ("TrailARN", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Values", InsightsMetricValues.to_json v.values)
       ; Some ("Timestamps", Timestamps.to_json v.timestamps)
       ; Aws.Util.option_map v.error_code (fun f -> "ErrorCode", String.to_json f)
       ; Aws.Util.option_map v.insight_type (fun f ->
             "InsightType", InsightType.to_json f)
       ; Aws.Util.option_map v.event_name (fun f -> "EventName", String.to_json f)
       ; Aws.Util.option_map v.event_source (fun f -> "EventSource", String.to_json f)
       ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
       ])

let of_json j =
  { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
  ; event_source = Aws.Util.option_map (Aws.Json.lookup j "EventSource") String.of_json
  ; event_name = Aws.Util.option_map (Aws.Json.lookup j "EventName") String.of_json
  ; insight_type =
      Aws.Util.option_map (Aws.Json.lookup j "InsightType") InsightType.of_json
  ; error_code = Aws.Util.option_map (Aws.Json.lookup j "ErrorCode") String.of_json
  ; timestamps =
      Timestamps.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamps"))
  ; values =
      InsightsMetricValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
