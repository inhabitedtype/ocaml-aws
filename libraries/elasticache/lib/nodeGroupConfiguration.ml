open Aws.BaseTypes

type t =
  { node_group_id : String.t option
  ; slots : String.t option
  ; replica_count : Integer.t option
  ; primary_availability_zone : String.t option
  ; replica_availability_zones : AvailabilityZonesList.t
  ; primary_outpost_arn : String.t option
  ; replica_outpost_arns : OutpostArnsList.t
  }

let make
    ?node_group_id
    ?slots
    ?replica_count
    ?primary_availability_zone
    ?(replica_availability_zones = [])
    ?primary_outpost_arn
    ?(replica_outpost_arns = [])
    () =
  { node_group_id
  ; slots
  ; replica_count
  ; primary_availability_zone
  ; replica_availability_zones
  ; primary_outpost_arn
  ; replica_outpost_arns
  }

let parse xml =
  Some
    { node_group_id = Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
    ; slots = Aws.Util.option_bind (Aws.Xml.member "Slots" xml) String.parse
    ; replica_count =
        Aws.Util.option_bind (Aws.Xml.member "ReplicaCount" xml) Integer.parse
    ; primary_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "PrimaryAvailabilityZone" xml) String.parse
    ; replica_availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicaAvailabilityZones" xml)
             AvailabilityZonesList.parse)
    ; primary_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "PrimaryOutpostArn" xml) String.parse
    ; replica_outpost_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicaOutpostArns" xml)
             OutpostArnsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReplicaOutpostArns.member"
              , OutpostArnsList.to_query v.replica_outpost_arns ))
       ; Aws.Util.option_map v.primary_outpost_arn (fun f ->
             Aws.Query.Pair ("PrimaryOutpostArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReplicaAvailabilityZones.member"
              , AvailabilityZonesList.to_query v.replica_availability_zones ))
       ; Aws.Util.option_map v.primary_availability_zone (fun f ->
             Aws.Query.Pair ("PrimaryAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.replica_count (fun f ->
             Aws.Query.Pair ("ReplicaCount", Integer.to_query f))
       ; Aws.Util.option_map v.slots (fun f ->
             Aws.Query.Pair ("Slots", String.to_query f))
       ; Aws.Util.option_map v.node_group_id (fun f ->
             Aws.Query.Pair ("NodeGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReplicaOutpostArns", OutpostArnsList.to_json v.replica_outpost_arns)
       ; Aws.Util.option_map v.primary_outpost_arn (fun f ->
             "PrimaryOutpostArn", String.to_json f)
       ; Some
           ( "ReplicaAvailabilityZones"
           , AvailabilityZonesList.to_json v.replica_availability_zones )
       ; Aws.Util.option_map v.primary_availability_zone (fun f ->
             "PrimaryAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.replica_count (fun f -> "ReplicaCount", Integer.to_json f)
       ; Aws.Util.option_map v.slots (fun f -> "Slots", String.to_json f)
       ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
       ])

let of_json j =
  { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
  ; slots = Aws.Util.option_map (Aws.Json.lookup j "Slots") String.of_json
  ; replica_count = Aws.Util.option_map (Aws.Json.lookup j "ReplicaCount") Integer.of_json
  ; primary_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "PrimaryAvailabilityZone") String.of_json
  ; replica_availability_zones =
      AvailabilityZonesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaAvailabilityZones"))
  ; primary_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "PrimaryOutpostArn") String.of_json
  ; replica_outpost_arns =
      OutpostArnsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaOutpostArns"))
  }
