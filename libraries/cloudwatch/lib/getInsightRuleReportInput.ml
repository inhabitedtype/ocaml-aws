open Aws.BaseTypes

type t =
  { rule_name : String.t
  ; start_time : DateTime.t
  ; end_time : DateTime.t
  ; period : Integer.t
  ; max_contributor_count : Integer.t option
  ; metrics : InsightRuleMetricList.t
  ; order_by : String.t option
  }

let make
    ~rule_name
    ~start_time
    ~end_time
    ~period
    ?max_contributor_count
    ?(metrics = [])
    ?order_by
    () =
  { rule_name; start_time; end_time; period; max_contributor_count; metrics; order_by }

let parse xml =
  Some
    { rule_name =
        Aws.Xml.required
          "RuleName"
          (Aws.Util.option_bind (Aws.Xml.member "RuleName" xml) String.parse)
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
    ; max_contributor_count =
        Aws.Util.option_bind (Aws.Xml.member "MaxContributorCount" xml) Integer.parse
    ; metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Metrics" xml)
             InsightRuleMetricList.parse)
    ; order_by = Aws.Util.option_bind (Aws.Xml.member "OrderBy" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.order_by (fun f ->
             Aws.Query.Pair ("OrderBy", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Metrics.member", InsightRuleMetricList.to_query v.metrics))
       ; Aws.Util.option_map v.max_contributor_count (fun f ->
             Aws.Query.Pair ("MaxContributorCount", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
       ; Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
       ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ; Some (Aws.Query.Pair ("RuleName", String.to_query v.rule_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.order_by (fun f -> "OrderBy", String.to_json f)
       ; Some ("Metrics", InsightRuleMetricList.to_json v.metrics)
       ; Aws.Util.option_map v.max_contributor_count (fun f ->
             "MaxContributorCount", Integer.to_json f)
       ; Some ("Period", Integer.to_json v.period)
       ; Some ("EndTime", DateTime.to_json v.end_time)
       ; Some ("StartTime", DateTime.to_json v.start_time)
       ; Some ("RuleName", String.to_json v.rule_name)
       ])

let of_json j =
  { rule_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleName"))
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
  ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
  ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
  ; max_contributor_count =
      Aws.Util.option_map (Aws.Json.lookup j "MaxContributorCount") Integer.of_json
  ; metrics =
      InsightRuleMetricList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
  ; order_by = Aws.Util.option_map (Aws.Json.lookup j "OrderBy") String.of_json
  }
