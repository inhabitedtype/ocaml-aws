open Aws.BaseTypes

type t =
  { nat_gateway_id : String.t
  ; allocation_ids : AllocationIdList.t
  ; private_ip_addresses : IpList.t
  ; dry_run : Boolean.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  }

let make
    ~nat_gateway_id
    ~allocation_ids
    ?(private_ip_addresses = [])
    ?dry_run
    ?availability_zone
    ?availability_zone_id
    () =
  { nat_gateway_id
  ; allocation_ids
  ; private_ip_addresses
  ; dry_run
  ; availability_zone
  ; availability_zone_id
  }

let parse xml =
  Some
    { nat_gateway_id =
        Aws.Xml.required
          "NatGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "NatGatewayId" xml) String.parse)
    ; allocation_ids =
        Aws.Xml.required
          "AllocationId"
          (Aws.Util.option_bind
             (Aws.Xml.member "AllocationId" xml)
             AllocationIdList.parse)
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) IpList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("PrivateIpAddress", IpList.to_query v.private_ip_addresses))
       ; Some
           (Aws.Query.Pair ("AllocationId", AllocationIdList.to_query v.allocation_ids))
       ; Some (Aws.Query.Pair ("NatGatewayId", String.to_query v.nat_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("PrivateIpAddress", IpList.to_json v.private_ip_addresses)
       ; Some ("AllocationId", AllocationIdList.to_json v.allocation_ids)
       ; Some ("NatGatewayId", String.to_json v.nat_gateway_id)
       ])

let of_json j =
  { nat_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NatGatewayId"))
  ; allocation_ids =
      AllocationIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AllocationId"))
  ; private_ip_addresses =
      IpList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrivateIpAddress"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  }
