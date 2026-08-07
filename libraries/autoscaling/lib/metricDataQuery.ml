open Aws.BaseTypes

type t =
  { id : String.t
  ; expression : String.t option
  ; metric_stat : MetricStat.t option
  ; label : String.t option
  ; return_data : Boolean.t option
  }

let make ~id ?expression ?metric_stat ?label ?return_data () =
  { id; expression; metric_stat; label; return_data }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; expression = Aws.Util.option_bind (Aws.Xml.member "Expression" xml) String.parse
    ; metric_stat =
        Aws.Util.option_bind (Aws.Xml.member "MetricStat" xml) MetricStat.parse
    ; label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
    ; return_data = Aws.Util.option_bind (Aws.Xml.member "ReturnData" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_data (fun f ->
             Aws.Query.Pair ("ReturnData", Boolean.to_query f))
       ; Aws.Util.option_map v.label (fun f ->
             Aws.Query.Pair ("Label", String.to_query f))
       ; Aws.Util.option_map v.metric_stat (fun f ->
             Aws.Query.Pair ("MetricStat", MetricStat.to_query f))
       ; Aws.Util.option_map v.expression (fun f ->
             Aws.Query.Pair ("Expression", String.to_query f))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_data (fun f -> "ReturnData", Boolean.to_json f)
       ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
       ; Aws.Util.option_map v.metric_stat (fun f -> "MetricStat", MetricStat.to_json f)
       ; Aws.Util.option_map v.expression (fun f -> "Expression", String.to_json f)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; expression = Aws.Util.option_map (Aws.Json.lookup j "Expression") String.of_json
  ; metric_stat = Aws.Util.option_map (Aws.Json.lookup j "MetricStat") MetricStat.of_json
  ; label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
  ; return_data = Aws.Util.option_map (Aws.Json.lookup j "ReturnData") Boolean.of_json
  }
