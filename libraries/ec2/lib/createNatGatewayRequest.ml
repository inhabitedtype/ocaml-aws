open Aws.BaseTypes

type t =
  { availability_mode : AvailabilityMode.t option
  ; allocation_id : String.t option
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; subnet_id : String.t option
  ; vpc_id : String.t option
  ; availability_zone_addresses : AvailabilityZoneAddresses.t
  ; tag_specifications : TagSpecificationList.t
  ; connectivity_type : ConnectivityType.t option
  ; private_ip_address : String.t option
  ; secondary_allocation_ids : AllocationIdList.t
  ; secondary_private_ip_addresses : IpList.t
  ; secondary_private_ip_address_count : Integer.t option
  }

let make
    ?availability_mode
    ?allocation_id
    ?client_token
    ?dry_run
    ?subnet_id
    ?vpc_id
    ?(availability_zone_addresses = [])
    ?(tag_specifications = [])
    ?connectivity_type
    ?private_ip_address
    ?(secondary_allocation_ids = [])
    ?(secondary_private_ip_addresses = [])
    ?secondary_private_ip_address_count
    () =
  { availability_mode
  ; allocation_id
  ; client_token
  ; dry_run
  ; subnet_id
  ; vpc_id
  ; availability_zone_addresses
  ; tag_specifications
  ; connectivity_type
  ; private_ip_address
  ; secondary_allocation_ids
  ; secondary_private_ip_addresses
  ; secondary_private_ip_address_count
  }

let parse xml =
  Some
    { availability_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "AvailabilityMode" xml)
          AvailabilityMode.parse
    ; allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "AllocationId" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; availability_zone_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZoneAddress" xml)
             AvailabilityZoneAddresses.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; connectivity_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectivityType" xml)
          ConnectivityType.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "PrivateIpAddress" xml) String.parse
    ; secondary_allocation_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecondaryAllocationId" xml)
             AllocationIdList.parse)
    ; secondary_private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecondaryPrivateIpAddress" xml)
             IpList.parse)
    ; secondary_private_ip_address_count =
        Aws.Util.option_bind
          (Aws.Xml.member "SecondaryPrivateIpAddressCount" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             Aws.Query.Pair ("SecondaryPrivateIpAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecondaryPrivateIpAddress"
              , IpList.to_query v.secondary_private_ip_addresses ))
       ; Some
           (Aws.Query.Pair
              ( "SecondaryAllocationId"
              , AllocationIdList.to_query v.secondary_allocation_ids ))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.connectivity_type (fun f ->
             Aws.Query.Pair ("ConnectivityType", ConnectivityType.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZoneAddress"
              , AvailabilityZoneAddresses.to_query v.availability_zone_addresses ))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ; Aws.Util.option_map v.availability_mode (fun f ->
             Aws.Query.Pair ("AvailabilityMode", AvailabilityMode.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             "SecondaryPrivateIpAddressCount", Integer.to_json f)
       ; Some
           ("SecondaryPrivateIpAddress", IpList.to_json v.secondary_private_ip_addresses)
       ; Some
           ("SecondaryAllocationId", AllocationIdList.to_json v.secondary_allocation_ids)
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "PrivateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.connectivity_type (fun f ->
             "ConnectivityType", ConnectivityType.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "AvailabilityZoneAddress"
           , AvailabilityZoneAddresses.to_json v.availability_zone_addresses )
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "AllocationId", String.to_json f)
       ; Aws.Util.option_map v.availability_mode (fun f ->
             "AvailabilityMode", AvailabilityMode.to_json f)
       ])

let of_json j =
  { availability_mode =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityMode") AvailabilityMode.of_json
  ; allocation_id = Aws.Util.option_map (Aws.Json.lookup j "AllocationId") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; availability_zone_addresses =
      AvailabilityZoneAddresses.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZoneAddress"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; connectivity_type =
      Aws.Util.option_map (Aws.Json.lookup j "ConnectivityType") ConnectivityType.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateIpAddress") String.of_json
  ; secondary_allocation_ids =
      AllocationIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecondaryAllocationId"))
  ; secondary_private_ip_addresses =
      IpList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecondaryPrivateIpAddress"))
  ; secondary_private_ip_address_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "SecondaryPrivateIpAddressCount")
        Integer.of_json
  }
