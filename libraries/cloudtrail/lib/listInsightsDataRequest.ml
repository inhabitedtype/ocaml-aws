open Aws.BaseTypes

type t =
  { insight_source : String.t
  ; data_type : ListInsightsDataType.t
  ; dimensions : ListInsightsDataDimensions.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ~insight_source
    ~data_type
    ?dimensions
    ?start_time
    ?end_time
    ?max_results
    ?next_token
    () =
  { insight_source; data_type; dimensions; start_time; end_time; max_results; next_token }

let parse xml =
  Some
    { insight_source =
        Aws.Xml.required
          "InsightSource"
          (Aws.Util.option_bind (Aws.Xml.member "InsightSource" xml) String.parse)
    ; data_type =
        Aws.Xml.required
          "DataType"
          (Aws.Util.option_bind
             (Aws.Xml.member "DataType" xml)
             ListInsightsDataType.parse)
    ; dimensions =
        Aws.Util.option_bind
          (Aws.Xml.member "Dimensions" xml)
          ListInsightsDataDimensions.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
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
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.dimensions (fun f ->
             Aws.Query.Pair ("Dimensions", ListInsightsDataDimensions.to_query f))
       ; Some (Aws.Query.Pair ("DataType", ListInsightsDataType.to_query v.data_type))
       ; Some (Aws.Query.Pair ("InsightSource", String.to_query v.insight_source))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.dimensions (fun f ->
             "Dimensions", ListInsightsDataDimensions.to_json f)
       ; Some ("DataType", ListInsightsDataType.to_json v.data_type)
       ; Some ("InsightSource", String.to_json v.insight_source)
       ])

let of_json j =
  { insight_source =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightSource"))
  ; data_type =
      ListInsightsDataType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DataType"))
  ; dimensions =
      Aws.Util.option_map
        (Aws.Json.lookup j "Dimensions")
        ListInsightsDataDimensions.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
