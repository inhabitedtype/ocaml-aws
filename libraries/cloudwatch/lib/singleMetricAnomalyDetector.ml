open Aws.BaseTypes

type t =
  { account_id : String.t option
  ; namespace : String.t option
  ; metric_name : String.t option
  ; dimensions : Dimensions.t
  ; stat : String.t option
  }

let make ?account_id ?namespace ?metric_name ?(dimensions = []) ?stat () =
  { account_id; namespace; metric_name; dimensions; stat }

let parse xml =
  Some
    { account_id = Aws.Util.option_bind (Aws.Xml.member "AccountId" xml) String.parse
    ; namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
    ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
    ; stat = Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stat (fun f -> Aws.Query.Pair ("Stat", String.to_query f))
       ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
       ; Aws.Util.option_map v.metric_name (fun f ->
             Aws.Query.Pair ("MetricName", String.to_query f))
       ; Aws.Util.option_map v.namespace (fun f ->
             Aws.Query.Pair ("Namespace", String.to_query f))
       ; Aws.Util.option_map v.account_id (fun f ->
             Aws.Query.Pair ("AccountId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stat (fun f -> "Stat", String.to_json f)
       ; Some ("Dimensions", Dimensions.to_json v.dimensions)
       ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
       ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
       ; Aws.Util.option_map v.account_id (fun f -> "AccountId", String.to_json f)
       ])

let of_json j =
  { account_id = Aws.Util.option_map (Aws.Json.lookup j "AccountId") String.of_json
  ; namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
  ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
  ; dimensions =
      Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; stat = Aws.Util.option_map (Aws.Json.lookup j "Stat") String.of_json
  }
