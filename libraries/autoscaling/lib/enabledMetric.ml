open Aws.BaseTypes

type t =
  { metric : String.t option
  ; granularity : String.t option
  }

let make ?metric ?granularity () = { metric; granularity }

let parse xml =
  Some
    { metric = Aws.Util.option_bind (Aws.Xml.member "Metric" xml) String.parse
    ; granularity = Aws.Util.option_bind (Aws.Xml.member "Granularity" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.granularity (fun f ->
             Aws.Query.Pair ("Granularity", String.to_query f))
       ; Aws.Util.option_map v.metric (fun f ->
             Aws.Query.Pair ("Metric", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.granularity (fun f -> "Granularity", String.to_json f)
       ; Aws.Util.option_map v.metric (fun f -> "Metric", String.to_json f)
       ])

let of_json j =
  { metric = Aws.Util.option_map (Aws.Json.lookup j "Metric") String.of_json
  ; granularity = Aws.Util.option_map (Aws.Json.lookup j "Granularity") String.of_json
  }
