open Aws.BaseTypes

type t =
  { traffic_policy_id_marker : String.t option
  ; max_items : String.t option
  }

let make ?traffic_policy_id_marker ?max_items () = { traffic_policy_id_marker; max_items }

let parse xml =
  Some
    { traffic_policy_id_marker =
        Aws.Util.option_bind (Aws.Xml.member "trafficpolicyid" xml) String.parse
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Aws.Util.option_map v.traffic_policy_id_marker (fun f ->
             Aws.Query.Pair ("trafficpolicyid", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Aws.Util.option_map v.traffic_policy_id_marker (fun f ->
             "trafficpolicyid", String.to_json f)
       ])

let of_json j =
  { traffic_policy_id_marker =
      Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyid") String.of_json
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  }
