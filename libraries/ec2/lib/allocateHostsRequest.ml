open Aws.BaseTypes

type t =
  { instance_family : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; host_recovery : HostRecovery.t option
  ; outpost_arn : String.t option
  ; host_maintenance : HostMaintenance.t option
  ; asset_ids : AssetIdList.t
  ; availability_zone_id : String.t option
  ; auto_placement : AutoPlacement.t option
  ; client_token : String.t option
  ; instance_type : String.t option
  ; quantity : Integer.t option
  ; availability_zone : String.t option
  }

let make
    ?instance_family
    ?(tag_specifications = [])
    ?host_recovery
    ?outpost_arn
    ?host_maintenance
    ?(asset_ids = [])
    ?availability_zone_id
    ?auto_placement
    ?client_token
    ?instance_type
    ?quantity
    ?availability_zone
    () =
  { instance_family
  ; tag_specifications
  ; host_recovery
  ; outpost_arn
  ; host_maintenance
  ; asset_ids
  ; availability_zone_id
  ; auto_placement
  ; client_token
  ; instance_type
  ; quantity
  ; availability_zone
  }

let parse xml =
  Some
    { instance_family =
        Aws.Util.option_bind (Aws.Xml.member "InstanceFamily" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; host_recovery =
        Aws.Util.option_bind (Aws.Xml.member "HostRecovery" xml) HostRecovery.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "OutpostArn" xml) String.parse
    ; host_maintenance =
        Aws.Util.option_bind (Aws.Xml.member "HostMaintenance" xml) HostMaintenance.parse
    ; asset_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AssetId" xml) AssetIdList.parse)
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; auto_placement =
        Aws.Util.option_bind (Aws.Xml.member "autoPlacement" xml) AutoPlacement.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; quantity = Aws.Util.option_bind (Aws.Xml.member "quantity" xml) Integer.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.quantity (fun f ->
             Aws.Query.Pair ("Quantity", Integer.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.auto_placement (fun f ->
             Aws.Query.Pair ("AutoPlacement", AutoPlacement.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Some (Aws.Query.Pair ("AssetId", AssetIdList.to_query v.asset_ids))
       ; Aws.Util.option_map v.host_maintenance (fun f ->
             Aws.Query.Pair ("HostMaintenance", HostMaintenance.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.host_recovery (fun f ->
             Aws.Query.Pair ("HostRecovery", HostRecovery.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.quantity (fun f -> "quantity", Integer.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.auto_placement (fun f ->
             "autoPlacement", AutoPlacement.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Some ("AssetId", AssetIdList.to_json v.asset_ids)
       ; Aws.Util.option_map v.host_maintenance (fun f ->
             "HostMaintenance", HostMaintenance.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "OutpostArn", String.to_json f)
       ; Aws.Util.option_map v.host_recovery (fun f ->
             "HostRecovery", HostRecovery.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.instance_family (fun f ->
             "InstanceFamily", String.to_json f)
       ])

let of_json j =
  { instance_family =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceFamily") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; host_recovery =
      Aws.Util.option_map (Aws.Json.lookup j "HostRecovery") HostRecovery.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "OutpostArn") String.of_json
  ; host_maintenance =
      Aws.Util.option_map (Aws.Json.lookup j "HostMaintenance") HostMaintenance.of_json
  ; asset_ids = AssetIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AssetId"))
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; auto_placement =
      Aws.Util.option_map (Aws.Json.lookup j "autoPlacement") AutoPlacement.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; quantity = Aws.Util.option_map (Aws.Json.lookup j "quantity") Integer.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  }
