open Aws.BaseTypes

type t =
  { metric : Metric.t
  ; period : Integer.t
  ; stat : String.t
  ; unit : StandardUnit.t option
  }

let make ~metric ~period ~stat ?unit () = { metric; period; stat; unit }

let parse xml =
  Some
    { metric =
        Aws.Xml.required
          "Metric"
          (Aws.Util.option_bind (Aws.Xml.member "Metric" xml) Metric.parse)
    ; period =
        Aws.Xml.required
          "Period"
          (Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse)
    ; stat =
        Aws.Xml.required
          "Stat"
          (Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse)
    ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.unit (fun f ->
             Aws.Query.Pair ("Unit", StandardUnit.to_query f))
       ; Some (Aws.Query.Pair ("Stat", String.to_query v.stat))
       ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
       ; Some (Aws.Query.Pair ("Metric", Metric.to_query v.metric))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
       ; Some ("Stat", String.to_json v.stat)
       ; Some ("Period", Integer.to_json v.period)
       ; Some ("Metric", Metric.to_json v.metric)
       ])

let of_json j =
  { metric = Metric.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metric"))
  ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
  ; stat = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Stat"))
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
  }
