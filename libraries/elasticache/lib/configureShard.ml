open Aws.BaseTypes

type t =
  { node_group_id : String.t
  ; new_replica_count : Integer.t
  ; preferred_availability_zones : PreferredAvailabilityZoneList.t
  ; preferred_outpost_arns : PreferredOutpostArnList.t
  }

let make
    ~node_group_id
    ~new_replica_count
    ?(preferred_availability_zones = [])
    ?(preferred_outpost_arns = [])
    () =
  { node_group_id
  ; new_replica_count
  ; preferred_availability_zones
  ; preferred_outpost_arns
  }

let parse xml =
  Some
    { node_group_id =
        Aws.Xml.required
          "NodeGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse)
    ; new_replica_count =
        Aws.Xml.required
          "NewReplicaCount"
          (Aws.Util.option_bind (Aws.Xml.member "NewReplicaCount" xml) Integer.parse)
    ; preferred_availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PreferredAvailabilityZones" xml)
             PreferredAvailabilityZoneList.parse)
    ; preferred_outpost_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PreferredOutpostArns" xml)
             PreferredOutpostArnList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PreferredOutpostArns.member"
              , PreferredOutpostArnList.to_query v.preferred_outpost_arns ))
       ; Some
           (Aws.Query.Pair
              ( "PreferredAvailabilityZones.member"
              , PreferredAvailabilityZoneList.to_query v.preferred_availability_zones ))
       ; Some (Aws.Query.Pair ("NewReplicaCount", Integer.to_query v.new_replica_count))
       ; Some (Aws.Query.Pair ("NodeGroupId", String.to_query v.node_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "PreferredOutpostArns"
           , PreferredOutpostArnList.to_json v.preferred_outpost_arns )
       ; Some
           ( "PreferredAvailabilityZones"
           , PreferredAvailabilityZoneList.to_json v.preferred_availability_zones )
       ; Some ("NewReplicaCount", Integer.to_json v.new_replica_count)
       ; Some ("NodeGroupId", String.to_json v.node_group_id)
       ])

let of_json j =
  { node_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupId"))
  ; new_replica_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NewReplicaCount"))
  ; preferred_availability_zones =
      PreferredAvailabilityZoneList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredAvailabilityZones"))
  ; preferred_outpost_arns =
      PreferredOutpostArnList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredOutpostArns"))
  }
