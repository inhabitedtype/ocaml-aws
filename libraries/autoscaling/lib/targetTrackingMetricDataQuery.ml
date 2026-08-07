open Aws.BaseTypes

type t =
  { id : String.t
  ; expression : String.t option
  ; metric_stat : TargetTrackingMetricStat.t option
  ; label : String.t option
  ; period : Integer.t option
  ; return_data : Boolean.t option
  }

let make ~id ?expression ?metric_stat ?label ?period ?return_data () =
  { id; expression; metric_stat; label; period; return_data }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; expression = Aws.Util.option_bind (Aws.Xml.member "Expression" xml) String.parse
    ; metric_stat =
        Aws.Util.option_bind
          (Aws.Xml.member "MetricStat" xml)
          TargetTrackingMetricStat.parse
    ; label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
    ; return_data = Aws.Util.option_bind (Aws.Xml.member "ReturnData" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_data (fun f ->
             Aws.Query.Pair ("ReturnData", Boolean.to_query f))
       ; Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", Integer.to_query f))
       ; Aws.Util.option_map v.label (fun f ->
             Aws.Query.Pair ("Label", String.to_query f))
       ; Aws.Util.option_map v.metric_stat (fun f ->
             Aws.Query.Pair ("MetricStat", TargetTrackingMetricStat.to_query f))
       ; Aws.Util.option_map v.expression (fun f ->
             Aws.Query.Pair ("Expression", String.to_query f))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_data (fun f -> "ReturnData", Boolean.to_json f)
       ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
       ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
       ; Aws.Util.option_map v.metric_stat (fun f ->
             "MetricStat", TargetTrackingMetricStat.to_json f)
       ; Aws.Util.option_map v.expression (fun f -> "Expression", String.to_json f)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; expression = Aws.Util.option_map (Aws.Json.lookup j "Expression") String.of_json
  ; metric_stat =
      Aws.Util.option_map
        (Aws.Json.lookup j "MetricStat")
        TargetTrackingMetricStat.of_json
  ; label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
  ; return_data = Aws.Util.option_map (Aws.Json.lookup j "ReturnData") Boolean.of_json
  }
