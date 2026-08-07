open Aws.BaseTypes

type t =
  { metric_names : MetricSet.t
  ; start_time : DateTime.t
  ; end_time : DateTime.t
  ; period : Integer.t
  ; group_by : GroupBySet.t
  ; filter_by : CapacityManagerConditionSet.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ~metric_names
    ~start_time
    ~end_time
    ~period
    ?(group_by = [])
    ?(filter_by = [])
    ?max_results
    ?next_token
    ?dry_run
    () =
  { metric_names
  ; start_time
  ; end_time
  ; period
  ; group_by
  ; filter_by
  ; max_results
  ; next_token
  ; dry_run
  }

let parse xml =
  Some
    { metric_names =
        Aws.Xml.required
          "MetricName"
          (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) MetricSet.parse)
    ; start_time =
        Aws.Xml.required
          "StartTime"
          (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
    ; end_time =
        Aws.Xml.required
          "EndTime"
          (Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse)
    ; period =
        Aws.Xml.required
          "Period"
          (Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse)
    ; group_by =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GroupBy" xml) GroupBySet.parse)
    ; filter_by =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "FilterBy" xml)
             CapacityManagerConditionSet.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("FilterBy", CapacityManagerConditionSet.to_query v.filter_by))
       ; Some (Aws.Query.Pair ("GroupBy", GroupBySet.to_query v.group_by))
       ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
       ; Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
       ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ; Some (Aws.Query.Pair ("MetricName", MetricSet.to_query v.metric_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("FilterBy", CapacityManagerConditionSet.to_json v.filter_by)
       ; Some ("GroupBy", GroupBySet.to_json v.group_by)
       ; Some ("Period", Integer.to_json v.period)
       ; Some ("EndTime", DateTime.to_json v.end_time)
       ; Some ("StartTime", DateTime.to_json v.start_time)
       ; Some ("MetricName", MetricSet.to_json v.metric_names)
       ])

let of_json j =
  { metric_names =
      MetricSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
  ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
  ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
  ; group_by = GroupBySet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupBy"))
  ; filter_by =
      CapacityManagerConditionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "FilterBy"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
