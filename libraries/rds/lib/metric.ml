open Aws.BaseTypes

type t =
  { name : String.t option
  ; references : MetricReferenceList.t
  ; statistics_details : String.t option
  ; metric_query : MetricQuery.t option
  }

let make ?name ?(references = []) ?statistics_details ?metric_query () =
  { name; references; statistics_details; metric_query }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; references =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "References" xml)
             MetricReferenceList.parse)
    ; statistics_details =
        Aws.Util.option_bind (Aws.Xml.member "StatisticsDetails" xml) String.parse
    ; metric_query =
        Aws.Util.option_bind (Aws.Xml.member "MetricQuery" xml) MetricQuery.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_query (fun f ->
             Aws.Query.Pair ("MetricQuery", MetricQuery.to_query f))
       ; Aws.Util.option_map v.statistics_details (fun f ->
             Aws.Query.Pair ("StatisticsDetails", String.to_query f))
       ; Some
           (Aws.Query.Pair ("References.member", MetricReferenceList.to_query v.references))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric_query (fun f ->
             "MetricQuery", MetricQuery.to_json f)
       ; Aws.Util.option_map v.statistics_details (fun f ->
             "StatisticsDetails", String.to_json f)
       ; Some ("References", MetricReferenceList.to_json v.references)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; references =
      MetricReferenceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "References"))
  ; statistics_details =
      Aws.Util.option_map (Aws.Json.lookup j "StatisticsDetails") String.of_json
  ; metric_query =
      Aws.Util.option_map (Aws.Json.lookup j "MetricQuery") MetricQuery.of_json
  }
