open Aws.BaseTypes

type t =
  { start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; metrics : MetricList.t
  ; analysis : String.t option
  }

let make ?start_time ?end_time ?(metrics = []) ?analysis () =
  { start_time; end_time; metrics; analysis }

let parse xml =
  Some
    { start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) MetricList.parse)
    ; analysis = Aws.Util.option_bind (Aws.Xml.member "Analysis" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.analysis (fun f ->
             Aws.Query.Pair ("Analysis", String.to_query f))
       ; Some (Aws.Query.Pair ("Metrics.member", MetricList.to_query v.metrics))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.analysis (fun f -> "Analysis", String.to_json f)
       ; Some ("Metrics", MetricList.to_json v.metrics)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ])

let of_json j =
  { start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; metrics = MetricList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
  ; analysis = Aws.Util.option_map (Aws.Json.lookup j "Analysis") String.of_json
  }
