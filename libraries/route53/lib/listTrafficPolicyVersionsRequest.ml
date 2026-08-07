open Aws.BaseTypes

type t =
  { id : String.t
  ; traffic_policy_version_marker : String.t option
  ; max_items : String.t option
  }

let make ~id ?traffic_policy_version_marker ?max_items () =
  { id; traffic_policy_version_marker; max_items }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; traffic_policy_version_marker =
        Aws.Util.option_bind (Aws.Xml.member "trafficpolicyversion" xml) String.parse
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Aws.Util.option_map v.traffic_policy_version_marker (fun f ->
             Aws.Query.Pair ("trafficpolicyversion", String.to_query f))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Aws.Util.option_map v.traffic_policy_version_marker (fun f ->
             "trafficpolicyversion", String.to_json f)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; traffic_policy_version_marker =
      Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyversion") String.of_json
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  }
