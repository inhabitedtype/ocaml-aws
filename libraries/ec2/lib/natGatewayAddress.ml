open Aws.BaseTypes

type t =
  { allocation_id : String.t option
  ; network_interface_id : String.t option
  ; private_ip : String.t option
  ; public_ip : String.t option
  ; association_id : String.t option
  ; is_primary : Boolean.t option
  ; failure_message : String.t option
  ; status : NatGatewayAddressStatus.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  }

let make
    ?allocation_id
    ?network_interface_id
    ?private_ip
    ?public_ip
    ?association_id
    ?is_primary
    ?failure_message
    ?status
    ?availability_zone
    ?availability_zone_id
    () =
  { allocation_id
  ; network_interface_id
  ; private_ip
  ; public_ip
  ; association_id
  ; is_primary
  ; failure_message
  ; status
  ; availability_zone
  ; availability_zone_id
  }

let parse xml =
  Some
    { allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "allocationId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; private_ip = Aws.Util.option_bind (Aws.Xml.member "privateIp" xml) String.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    ; association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; is_primary = Aws.Util.option_bind (Aws.Xml.member "isPrimary" xml) Boolean.parse
    ; failure_message =
        Aws.Util.option_bind (Aws.Xml.member "failureMessage" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) NatGatewayAddressStatus.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", NatGatewayAddressStatus.to_query f))
       ; Aws.Util.option_map v.failure_message (fun f ->
             Aws.Query.Pair ("FailureMessage", String.to_query f))
       ; Aws.Util.option_map v.is_primary (fun f ->
             Aws.Query.Pair ("IsPrimary", Boolean.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.private_ip (fun f ->
             Aws.Query.Pair ("PrivateIp", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", NatGatewayAddressStatus.to_json f)
       ; Aws.Util.option_map v.failure_message (fun f ->
             "failureMessage", String.to_json f)
       ; Aws.Util.option_map v.is_primary (fun f -> "isPrimary", Boolean.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ; Aws.Util.option_map v.private_ip (fun f -> "privateIp", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "allocationId", String.to_json f)
       ])

let of_json j =
  { allocation_id = Aws.Util.option_map (Aws.Json.lookup j "allocationId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; private_ip = Aws.Util.option_map (Aws.Json.lookup j "privateIp") String.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  ; association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; is_primary = Aws.Util.option_map (Aws.Json.lookup j "isPrimary") Boolean.of_json
  ; failure_message =
      Aws.Util.option_map (Aws.Json.lookup j "failureMessage") String.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") NatGatewayAddressStatus.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
