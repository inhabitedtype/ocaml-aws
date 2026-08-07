open Aws.BaseTypes

type t =
  { id : String.t option
  ; source : String.t option
  ; destination : String.t option
  ; metric : MetricType.t option
  ; statistic : StatisticType.t option
  ; period : PeriodType.t option
  }

let make ?id ?source ?destination ?metric ?statistic ?period () =
  { id; source; destination; metric; statistic; period }

let parse xml =
  Some
    { id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse
    ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
    ; destination = Aws.Util.option_bind (Aws.Xml.member "Destination" xml) String.parse
    ; metric = Aws.Util.option_bind (Aws.Xml.member "Metric" xml) MetricType.parse
    ; statistic =
        Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) StatisticType.parse
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) PeriodType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", PeriodType.to_query f))
       ; Aws.Util.option_map v.statistic (fun f ->
             Aws.Query.Pair ("Statistic", StatisticType.to_query f))
       ; Aws.Util.option_map v.metric (fun f ->
             Aws.Query.Pair ("Metric", MetricType.to_query f))
       ; Aws.Util.option_map v.destination (fun f ->
             Aws.Query.Pair ("Destination", String.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", String.to_query f))
       ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.period (fun f -> "Period", PeriodType.to_json f)
       ; Aws.Util.option_map v.statistic (fun f -> "Statistic", StatisticType.to_json f)
       ; Aws.Util.option_map v.metric (fun f -> "Metric", MetricType.to_json f)
       ; Aws.Util.option_map v.destination (fun f -> "Destination", String.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "Id", String.to_json f)
       ])

let of_json j =
  { id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
  ; destination = Aws.Util.option_map (Aws.Json.lookup j "Destination") String.of_json
  ; metric = Aws.Util.option_map (Aws.Json.lookup j "Metric") MetricType.of_json
  ; statistic = Aws.Util.option_map (Aws.Json.lookup j "Statistic") StatisticType.of_json
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") PeriodType.of_json
  }
