open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; traffic_policy_instance_name_marker : String.t option
  ; traffic_policy_instance_type_marker : RRType.t option
  ; max_items : String.t option
  }

let make
    ~hosted_zone_id
    ?traffic_policy_instance_name_marker
    ?traffic_policy_instance_type_marker
    ?max_items
    () =
  { hosted_zone_id
  ; traffic_policy_instance_name_marker
  ; traffic_policy_instance_type_marker
  ; max_items
  }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "id"
          (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse)
    ; traffic_policy_instance_name_marker =
        Aws.Util.option_bind (Aws.Xml.member "trafficpolicyinstancename" xml) String.parse
    ; traffic_policy_instance_type_marker =
        Aws.Util.option_bind (Aws.Xml.member "trafficpolicyinstancetype" xml) RRType.parse
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
             Aws.Query.Pair ("trafficpolicyinstancetype", RRType.to_query f))
       ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
             Aws.Query.Pair ("trafficpolicyinstancename", String.to_query f))
       ; Some (Aws.Query.Pair ("id", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Aws.Util.option_map v.traffic_policy_instance_type_marker (fun f ->
             "trafficpolicyinstancetype", RRType.to_json f)
       ; Aws.Util.option_map v.traffic_policy_instance_name_marker (fun f ->
             "trafficpolicyinstancename", String.to_json f)
       ; Some ("id", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id"))
  ; traffic_policy_instance_name_marker =
      Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancename") String.of_json
  ; traffic_policy_instance_type_marker =
      Aws.Util.option_map (Aws.Json.lookup j "trafficpolicyinstancetype") RRType.of_json
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  }
