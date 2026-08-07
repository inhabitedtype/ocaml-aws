open Aws.BaseTypes

type t =
  { metric_data_queries : MetricDataQueries.t
  ; start_time : DateTime.t
  ; end_time : DateTime.t
  ; next_token : String.t option
  ; scan_by : ScanBy.t option
  ; max_datapoints : Integer.t option
  ; label_options : LabelOptions.t option
  }

let make
    ~metric_data_queries
    ~start_time
    ~end_time
    ?next_token
    ?scan_by
    ?max_datapoints
    ?label_options
    () =
  { metric_data_queries
  ; start_time
  ; end_time
  ; next_token
  ; scan_by
  ; max_datapoints
  ; label_options
  }

let parse xml =
  Some
    { metric_data_queries =
        Aws.Xml.required
          "MetricDataQueries"
          (Aws.Util.option_bind
             (Aws.Xml.member "MetricDataQueries" xml)
             MetricDataQueries.parse)
    ; start_time =
        Aws.Xml.required
          "StartTime"
          (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
    ; end_time =
        Aws.Xml.required
          "EndTime"
          (Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; scan_by = Aws.Util.option_bind (Aws.Xml.member "ScanBy" xml) ScanBy.parse
    ; max_datapoints =
        Aws.Util.option_bind (Aws.Xml.member "MaxDatapoints" xml) Integer.parse
    ; label_options =
        Aws.Util.option_bind (Aws.Xml.member "LabelOptions" xml) LabelOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.label_options (fun f ->
             Aws.Query.Pair ("LabelOptions", LabelOptions.to_query f))
       ; Aws.Util.option_map v.max_datapoints (fun f ->
             Aws.Query.Pair ("MaxDatapoints", Integer.to_query f))
       ; Aws.Util.option_map v.scan_by (fun f ->
             Aws.Query.Pair ("ScanBy", ScanBy.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
       ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ; Some
           (Aws.Query.Pair
              ( "MetricDataQueries.member"
              , MetricDataQueries.to_query v.metric_data_queries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.label_options (fun f ->
             "LabelOptions", LabelOptions.to_json f)
       ; Aws.Util.option_map v.max_datapoints (fun f ->
             "MaxDatapoints", Integer.to_json f)
       ; Aws.Util.option_map v.scan_by (fun f -> "ScanBy", ScanBy.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("EndTime", DateTime.to_json v.end_time)
       ; Some ("StartTime", DateTime.to_json v.start_time)
       ; Some ("MetricDataQueries", MetricDataQueries.to_json v.metric_data_queries)
       ])

let of_json j =
  { metric_data_queries =
      MetricDataQueries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricDataQueries"))
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
  ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; scan_by = Aws.Util.option_map (Aws.Json.lookup j "ScanBy") ScanBy.of_json
  ; max_datapoints =
      Aws.Util.option_map (Aws.Json.lookup j "MaxDatapoints") Integer.of_json
  ; label_options =
      Aws.Util.option_map (Aws.Json.lookup j "LabelOptions") LabelOptions.of_json
  }
