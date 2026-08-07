open Aws.BaseTypes

type t =
  { id : String.t
  ; metric_stat : MetricStat.t option
  ; expression : String.t option
  ; label : String.t option
  ; return_data : Boolean.t option
  ; period : Integer.t option
  ; account_id : String.t option
  }

let make ~id ?metric_stat ?expression ?label ?return_data ?period ?account_id () =
  { id; metric_stat; expression; label; return_data; period; account_id }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; metric_stat =
        Aws.Util.option_bind (Aws.Xml.member "MetricStat" xml) MetricStat.parse
    ; expression = Aws.Util.option_bind (Aws.Xml.member "Expression" xml) String.parse
    ; label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
    ; return_data = Aws.Util.option_bind (Aws.Xml.member "ReturnData" xml) Boolean.parse
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
    ; account_id = Aws.Util.option_bind (Aws.Xml.member "AccountId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account_id (fun f ->
             Aws.Query.Pair ("AccountId", String.to_query f))
       ; Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", Integer.to_query f))
       ; Aws.Util.option_map v.return_data (fun f ->
             Aws.Query.Pair ("ReturnData", Boolean.to_query f))
       ; Aws.Util.option_map v.label (fun f ->
             Aws.Query.Pair ("Label", String.to_query f))
       ; Aws.Util.option_map v.expression (fun f ->
             Aws.Query.Pair ("Expression", String.to_query f))
       ; Aws.Util.option_map v.metric_stat (fun f ->
             Aws.Query.Pair ("MetricStat", MetricStat.to_query f))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account_id (fun f -> "AccountId", String.to_json f)
       ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
       ; Aws.Util.option_map v.return_data (fun f -> "ReturnData", Boolean.to_json f)
       ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
       ; Aws.Util.option_map v.expression (fun f -> "Expression", String.to_json f)
       ; Aws.Util.option_map v.metric_stat (fun f -> "MetricStat", MetricStat.to_json f)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; metric_stat = Aws.Util.option_map (Aws.Json.lookup j "MetricStat") MetricStat.of_json
  ; expression = Aws.Util.option_map (Aws.Json.lookup j "Expression") String.of_json
  ; label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
  ; return_data = Aws.Util.option_map (Aws.Json.lookup j "ReturnData") Boolean.of_json
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
  ; account_id = Aws.Util.option_map (Aws.Json.lookup j "AccountId") String.of_json
  }
