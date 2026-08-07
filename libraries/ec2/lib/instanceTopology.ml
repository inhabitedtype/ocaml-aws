open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; instance_type : String.t option
  ; group_name : String.t option
  ; network_nodes : NetworkNodesList.t
  ; availability_zone : String.t option
  ; zone_id : String.t option
  ; capacity_block_id : String.t option
  }

let make
    ?instance_id
    ?instance_type
    ?group_name
    ?(network_nodes = [])
    ?availability_zone
    ?zone_id
    ?capacity_block_id
    () =
  { instance_id
  ; instance_type
  ; group_name
  ; network_nodes
  ; availability_zone
  ; zone_id
  ; capacity_block_id
  }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; network_nodes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkNodeSet" xml)
             NetworkNodesList.parse)
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; zone_id = Aws.Util.option_bind (Aws.Xml.member "zoneId" xml) String.parse
    ; capacity_block_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityBlockId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_block_id (fun f ->
             Aws.Query.Pair ("CapacityBlockId", String.to_query f))
       ; Aws.Util.option_map v.zone_id (fun f ->
             Aws.Query.Pair ("ZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Some
           (Aws.Query.Pair ("NetworkNodeSet", NetworkNodesList.to_query v.network_nodes))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_block_id (fun f ->
             "capacityBlockId", String.to_json f)
       ; Aws.Util.option_map v.zone_id (fun f -> "zoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Some ("networkNodeSet", NetworkNodesList.to_json v.network_nodes)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; network_nodes =
      NetworkNodesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkNodeSet"))
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; zone_id = Aws.Util.option_map (Aws.Json.lookup j "zoneId") String.of_json
  ; capacity_block_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockId") String.of_json
  }
