open Aws.BaseTypes

type t =
  { metric : Metric.t
  ; stat : String.t
  ; unit : String.t option
  ; period : Integer.t option
  }

let make ~metric ~stat ?unit ?period () = { metric; stat; unit; period }

let parse xml =
  Some
    { metric =
        Aws.Xml.required
          "Metric"
          (Aws.Util.option_bind (Aws.Xml.member "Metric" xml) Metric.parse)
    ; stat =
        Aws.Xml.required
          "Stat"
          (Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse)
    ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) String.parse
    ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.period (fun f ->
             Aws.Query.Pair ("Period", Integer.to_query f))
       ; Aws.Util.option_map v.unit (fun f -> Aws.Query.Pair ("Unit", String.to_query f))
       ; Some (Aws.Query.Pair ("Stat", String.to_query v.stat))
       ; Some (Aws.Query.Pair ("Metric", Metric.to_query v.metric))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
       ; Aws.Util.option_map v.unit (fun f -> "Unit", String.to_json f)
       ; Some ("Stat", String.to_json v.stat)
       ; Some ("Metric", Metric.to_json v.metric)
       ])

let of_json j =
  { metric = Metric.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metric"))
  ; stat = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Stat"))
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") String.of_json
  ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
  }
