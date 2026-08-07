open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t option
  ; capacity_block_id : String.t option
  ; state : String.t option
  ; instance_type : String.t option
  ; group_name : String.t option
  ; network_nodes : NetworkNodeSet.t
  ; availability_zone_id : String.t option
  ; availability_zone : String.t option
  }

let make
    ?capacity_reservation_id
    ?capacity_block_id
    ?state
    ?instance_type
    ?group_name
    ?(network_nodes = [])
    ?availability_zone_id
    ?availability_zone
    () =
  { capacity_reservation_id
  ; capacity_block_id
  ; state
  ; instance_type
  ; group_name
  ; network_nodes
  ; availability_zone_id
  ; availability_zone
  }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; capacity_block_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityBlockId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; network_nodes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkNodeSet" xml)
             NetworkNodeSet.parse)
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Some (Aws.Query.Pair ("NetworkNodeSet", NetworkNodeSet.to_query v.network_nodes))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             Aws.Query.Pair ("CapacityBlockId", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Some ("networkNodeSet", NetworkNodeSet.to_json v.network_nodes)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             "capacityBlockId", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; capacity_block_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; network_nodes =
      NetworkNodeSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkNodeSet"))
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  }
