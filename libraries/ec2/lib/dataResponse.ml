open Aws.BaseTypes

type t =
  { id : String.t option
  ; source : String.t option
  ; destination : String.t option
  ; metric : MetricType.t option
  ; statistic : StatisticType.t option
  ; period : PeriodType.t option
  ; metric_points : MetricPoints.t
  }

let make ?id ?source ?destination ?metric ?statistic ?period ?(metric_points = []) () =
  { id; source; destination; metric; statistic; period; metric_points }

let parse xml =
  Some
    { id = Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse
    ; source = Aws.Util.option_bind (Aws.Xml.member "source" xml) String.parse
    ; destination = Aws.Util.option_bind (Aws.Xml.member "destination" xml) String.parse
    ; metric = Aws.Util.option_bind (Aws.Xml.member "metric" xml) MetricType.parse
    ; statistic =
        Aws.Util.option_bind (Aws.Xml.member "statistic" xml) StatisticType.parse
    ; period = Aws.Util.option_bind (Aws.Xml.member "period" xml) PeriodType.parse
    ; metric_points =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "metricPointSet" xml) MetricPoints.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MetricPointSet", MetricPoints.to_query v.metric_points))
       ; Aws.Util.option_map v.period (fun f ->
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
       [ Some ("metricPointSet", MetricPoints.to_json v.metric_points)
       ; Aws.Util.option_map v.period (fun f -> "period", PeriodType.to_json f)
       ; Aws.Util.option_map v.statistic (fun f -> "statistic", StatisticType.to_json f)
       ; Aws.Util.option_map v.metric (fun f -> "metric", MetricType.to_json f)
       ; Aws.Util.option_map v.destination (fun f -> "destination", String.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "source", String.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "id", String.to_json f)
       ])

let of_json j =
  { id = Aws.Util.option_map (Aws.Json.lookup j "id") String.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "source") String.of_json
  ; destination = Aws.Util.option_map (Aws.Json.lookup j "destination") String.of_json
  ; metric = Aws.Util.option_map (Aws.Json.lookup j "metric") MetricType.of_json
  ; statistic = Aws.Util.option_map (Aws.Json.lookup j "statistic") StatisticType.of_json
  ; period = Aws.Util.option_map (Aws.Json.lookup j "period") PeriodType.of_json
  ; metric_points =
      MetricPoints.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "metricPointSet"))
  }
