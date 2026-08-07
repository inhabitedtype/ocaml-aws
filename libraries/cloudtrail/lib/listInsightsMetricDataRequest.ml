open Aws.BaseTypes

type t =
  { trail_name : String.t option
  ; event_source : String.t
  ; event_name : String.t
  ; insight_type : InsightType.t
  ; error_code : String.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; period : Integer.t option
  ; data_type : InsightsMetricDataType.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?trail_name
    ~event_source
    ~event_name
    ~insight_type
    ?error_code
    ?start_time
    ?end_time
    ?period
    ?data_type
    ?max_results
    ?next_token
    () =
  { trail_name
  ; event_source
  ; event_name
  ; insight_type
  ; error_code
  ; start_time
  ; end_time
  ; period
  ; data_type
  ; max_results
  ; next_token
  }

let parse xml =
  Some
    { trail_name = Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse
    ; event_source =
        Aws.Xml.required
          "EventSource"
          (Aws.Util.option_bind (Aws.Xml.member "EventSource" xml) String.parse)
    ; event_name =
        Aws.Xml.required
          "EventName"
          (Aws.Util.option_bind (Aws.Xml.member "EventName" xml) String.parse)
    ; insight_type =
        Aws.Xml.required
          "InsightType"
          (Aws.Util.option_bind (Aws.Xml.member "InsightType" xml) InsightType.parse)
    ; error_code = Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) String.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
    ; data_type =
        Aws.Util.option_bind (Aws.Xml.member "DataType" xml) InsightsMetricDataType.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.data_type (fun f ->
             Aws.Query.Pair ("DataType", InsightsMetricDataType.to_query f))
       ; Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", Integer.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.error_code (fun f ->
             Aws.Query.Pair ("ErrorCode", String.to_query f))
       ; Some (Aws.Query.Pair ("InsightType", InsightType.to_query v.insight_type))
       ; Some (Aws.Query.Pair ("EventName", String.to_query v.event_name))
       ; Some (Aws.Query.Pair ("EventSource", String.to_query v.event_source))
       ; Aws.Util.option_map v.trail_name (fun f ->
             Aws.Query.Pair ("TrailName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.data_type (fun f ->
             "DataType", InsightsMetricDataType.to_json f)
       ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.error_code (fun f -> "ErrorCode", String.to_json f)
       ; Some ("InsightType", InsightType.to_json v.insight_type)
       ; Some ("EventName", String.to_json v.event_name)
       ; Some ("EventSource", String.to_json v.event_source)
       ; Aws.Util.option_map v.trail_name (fun f -> "TrailName", String.to_json f)
       ])

let of_json j =
  { trail_name = Aws.Util.option_map (Aws.Json.lookup j "TrailName") String.of_json
  ; event_source =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EventSource"))
  ; event_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EventName"))
  ; insight_type =
      InsightType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightType"))
  ; error_code = Aws.Util.option_map (Aws.Json.lookup j "ErrorCode") String.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
  ; data_type =
      Aws.Util.option_map (Aws.Json.lookup j "DataType") InsightsMetricDataType.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
