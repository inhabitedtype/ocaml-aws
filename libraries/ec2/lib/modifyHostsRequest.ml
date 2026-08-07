open Aws.BaseTypes

type t =
  { host_recovery : HostRecovery.t option
  ; instance_type : String.t option
  ; instance_family : String.t option
  ; host_maintenance : HostMaintenance.t option
  ; host_ids : RequestHostIdList.t
  ; auto_placement : AutoPlacement.t option
  }

let make
    ?host_recovery
    ?instance_type
    ?instance_family
    ?host_maintenance
    ~host_ids
    ?auto_placement
    () =
  { host_recovery
  ; instance_type
  ; instance_family
  ; host_maintenance
  ; host_ids
  ; auto_placement
  }

let parse xml =
  Some
    { host_recovery =
        Aws.Util.option_bind (Aws.Xml.member "HostRecovery" xml) HostRecovery.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse
    ; instance_family =
        Aws.Util.option_bind (Aws.Xml.member "InstanceFamily" xml) String.parse
    ; host_maintenance =
        Aws.Util.option_bind (Aws.Xml.member "HostMaintenance" xml) HostMaintenance.parse
    ; host_ids =
        Aws.Xml.required
          "hostId"
          (Aws.Util.option_bind (Aws.Xml.member "hostId" xml) RequestHostIdList.parse)
    ; auto_placement =
        Aws.Util.option_bind (Aws.Xml.member "autoPlacement" xml) AutoPlacement.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_placement (fun f ->
             Aws.Query.Pair ("AutoPlacement", AutoPlacement.to_query f))
       ; Some (Aws.Query.Pair ("HostId", RequestHostIdList.to_query v.host_ids))
       ; Aws.Util.option_map v.host_maintenance (fun f ->
             Aws.Query.Pair ("HostMaintenance", HostMaintenance.to_query f))
       ; Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.host_recovery (fun f ->
             Aws.Query.Pair ("HostRecovery", HostRecovery.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_placement (fun f ->
             "autoPlacement", AutoPlacement.to_json f)
       ; Some ("hostId", RequestHostIdList.to_json v.host_ids)
       ; Aws.Util.option_map v.host_maintenance (fun f ->
             "HostMaintenance", HostMaintenance.to_json f)
       ; Aws.Util.option_map v.instance_family (fun f ->
             "InstanceFamily", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "InstanceType", String.to_json f)
       ; Aws.Util.option_map v.host_recovery (fun f ->
             "HostRecovery", HostRecovery.to_json f)
       ])

let of_json j =
  { host_recovery =
      Aws.Util.option_map (Aws.Json.lookup j "HostRecovery") HostRecovery.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "InstanceType") String.of_json
  ; instance_family =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceFamily") String.of_json
  ; host_maintenance =
      Aws.Util.option_map (Aws.Json.lookup j "HostMaintenance") HostMaintenance.of_json
  ; host_ids =
      RequestHostIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostId"))
  ; auto_placement =
      Aws.Util.option_map (Aws.Json.lookup j "autoPlacement") AutoPlacement.of_json
  }
