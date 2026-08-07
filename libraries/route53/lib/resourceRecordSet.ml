open Aws.BaseTypes

type t =
  { name : String.t
  ; type_ : RRType.t
  ; set_identifier : String.t option
  ; weight : Long.t option
  ; region : ResourceRecordSetRegion.t option
  ; geo_location : GeoLocation.t option
  ; failover : ResourceRecordSetFailover.t option
  ; multi_value_answer : Boolean.t option
  ; t_t_l : Long.t option
  ; resource_records : ResourceRecords.t
  ; alias_target : AliasTarget.t option
  ; health_check_id : String.t option
  ; traffic_policy_instance_id : String.t option
  ; cidr_routing_config : CidrRoutingConfig.t option
  ; geo_proximity_location : GeoProximityLocation.t option
  }

let make
    ~name
    ~type_
    ?set_identifier
    ?weight
    ?region
    ?geo_location
    ?failover
    ?multi_value_answer
    ?t_t_l
    ?(resource_records = [])
    ?alias_target
    ?health_check_id
    ?traffic_policy_instance_id
    ?cidr_routing_config
    ?geo_proximity_location
    () =
  { name
  ; type_
  ; set_identifier
  ; weight
  ; region
  ; geo_location
  ; failover
  ; multi_value_answer
  ; t_t_l
  ; resource_records
  ; alias_target
  ; health_check_id
  ; traffic_policy_instance_id
  ; cidr_routing_config
  ; geo_proximity_location
  }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) RRType.parse)
    ; set_identifier =
        Aws.Util.option_bind (Aws.Xml.member "SetIdentifier" xml) String.parse
    ; weight = Aws.Util.option_bind (Aws.Xml.member "Weight" xml) Long.parse
    ; region =
        Aws.Util.option_bind (Aws.Xml.member "Region" xml) ResourceRecordSetRegion.parse
    ; geo_location =
        Aws.Util.option_bind (Aws.Xml.member "GeoLocation" xml) GeoLocation.parse
    ; failover =
        Aws.Util.option_bind
          (Aws.Xml.member "Failover" xml)
          ResourceRecordSetFailover.parse
    ; multi_value_answer =
        Aws.Util.option_bind (Aws.Xml.member "MultiValueAnswer" xml) Boolean.parse
    ; t_t_l = Aws.Util.option_bind (Aws.Xml.member "TTL" xml) Long.parse
    ; resource_records =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceRecords" xml)
             ResourceRecords.parse)
    ; alias_target =
        Aws.Util.option_bind (Aws.Xml.member "AliasTarget" xml) AliasTarget.parse
    ; health_check_id =
        Aws.Util.option_bind (Aws.Xml.member "HealthCheckId" xml) String.parse
    ; traffic_policy_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "TrafficPolicyInstanceId" xml) String.parse
    ; cidr_routing_config =
        Aws.Util.option_bind
          (Aws.Xml.member "CidrRoutingConfig" xml)
          CidrRoutingConfig.parse
    ; geo_proximity_location =
        Aws.Util.option_bind
          (Aws.Xml.member "GeoProximityLocation" xml)
          GeoProximityLocation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.geo_proximity_location (fun f ->
             Aws.Query.Pair ("GeoProximityLocation", GeoProximityLocation.to_query f))
       ; Aws.Util.option_map v.cidr_routing_config (fun f ->
             Aws.Query.Pair ("CidrRoutingConfig", CidrRoutingConfig.to_query f))
       ; Aws.Util.option_map v.traffic_policy_instance_id (fun f ->
             Aws.Query.Pair ("TrafficPolicyInstanceId", String.to_query f))
       ; Aws.Util.option_map v.health_check_id (fun f ->
             Aws.Query.Pair ("HealthCheckId", String.to_query f))
       ; Aws.Util.option_map v.alias_target (fun f ->
             Aws.Query.Pair ("AliasTarget", AliasTarget.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceRecords.member", ResourceRecords.to_query v.resource_records))
       ; Aws.Util.option_map v.t_t_l (fun f -> Aws.Query.Pair ("TTL", Long.to_query f))
       ; Aws.Util.option_map v.multi_value_answer (fun f ->
             Aws.Query.Pair ("MultiValueAnswer", Boolean.to_query f))
       ; Aws.Util.option_map v.failover (fun f ->
             Aws.Query.Pair ("Failover", ResourceRecordSetFailover.to_query f))
       ; Aws.Util.option_map v.geo_location (fun f ->
             Aws.Query.Pair ("GeoLocation", GeoLocation.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", ResourceRecordSetRegion.to_query f))
       ; Aws.Util.option_map v.weight (fun f ->
             Aws.Query.Pair ("Weight", Long.to_query f))
       ; Aws.Util.option_map v.set_identifier (fun f ->
             Aws.Query.Pair ("SetIdentifier", String.to_query f))
       ; Some (Aws.Query.Pair ("Type", RRType.to_query v.type_))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.geo_proximity_location (fun f ->
             "GeoProximityLocation", GeoProximityLocation.to_json f)
       ; Aws.Util.option_map v.cidr_routing_config (fun f ->
             "CidrRoutingConfig", CidrRoutingConfig.to_json f)
       ; Aws.Util.option_map v.traffic_policy_instance_id (fun f ->
             "TrafficPolicyInstanceId", String.to_json f)
       ; Aws.Util.option_map v.health_check_id (fun f ->
             "HealthCheckId", String.to_json f)
       ; Aws.Util.option_map v.alias_target (fun f ->
             "AliasTarget", AliasTarget.to_json f)
       ; Some ("ResourceRecords", ResourceRecords.to_json v.resource_records)
       ; Aws.Util.option_map v.t_t_l (fun f -> "TTL", Long.to_json f)
       ; Aws.Util.option_map v.multi_value_answer (fun f ->
             "MultiValueAnswer", Boolean.to_json f)
       ; Aws.Util.option_map v.failover (fun f ->
             "Failover", ResourceRecordSetFailover.to_json f)
       ; Aws.Util.option_map v.geo_location (fun f ->
             "GeoLocation", GeoLocation.to_json f)
       ; Aws.Util.option_map v.region (fun f ->
             "Region", ResourceRecordSetRegion.to_json f)
       ; Aws.Util.option_map v.weight (fun f -> "Weight", Long.to_json f)
       ; Aws.Util.option_map v.set_identifier (fun f -> "SetIdentifier", String.to_json f)
       ; Some ("Type", RRType.to_json v.type_)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; type_ = RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; set_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SetIdentifier") String.of_json
  ; weight = Aws.Util.option_map (Aws.Json.lookup j "Weight") Long.of_json
  ; region =
      Aws.Util.option_map (Aws.Json.lookup j "Region") ResourceRecordSetRegion.of_json
  ; geo_location =
      Aws.Util.option_map (Aws.Json.lookup j "GeoLocation") GeoLocation.of_json
  ; failover =
      Aws.Util.option_map (Aws.Json.lookup j "Failover") ResourceRecordSetFailover.of_json
  ; multi_value_answer =
      Aws.Util.option_map (Aws.Json.lookup j "MultiValueAnswer") Boolean.of_json
  ; t_t_l = Aws.Util.option_map (Aws.Json.lookup j "TTL") Long.of_json
  ; resource_records =
      ResourceRecords.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRecords"))
  ; alias_target =
      Aws.Util.option_map (Aws.Json.lookup j "AliasTarget") AliasTarget.of_json
  ; health_check_id =
      Aws.Util.option_map (Aws.Json.lookup j "HealthCheckId") String.of_json
  ; traffic_policy_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "TrafficPolicyInstanceId") String.of_json
  ; cidr_routing_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "CidrRoutingConfig")
        CidrRoutingConfig.of_json
  ; geo_proximity_location =
      Aws.Util.option_map
        (Aws.Json.lookup j "GeoProximityLocation")
        GeoProximityLocation.of_json
  }
