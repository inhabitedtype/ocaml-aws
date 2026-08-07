open Aws.BaseTypes

type t =
  { tag_specifications : TagSpecificationList.t
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; cidr_block : String.t option
  ; ipv6_cidr_block : String.t option
  ; outpost_arn : String.t option
  ; vpc_id : String.t
  ; ipv6_native : Boolean.t option
  ; ipv4_ipam_pool_id : String.t option
  ; ipv4_netmask_length : Integer.t option
  ; ipv6_ipam_pool_id : String.t option
  ; ipv6_netmask_length : Integer.t option
  ; dry_run : Boolean.t option
  }

let make
    ?(tag_specifications = [])
    ?availability_zone
    ?availability_zone_id
    ?cidr_block
    ?ipv6_cidr_block
    ?outpost_arn
    ~vpc_id
    ?ipv6_native
    ?ipv4_ipam_pool_id
    ?ipv4_netmask_length
    ?ipv6_ipam_pool_id
    ?ipv6_netmask_length
    ?dry_run
    () =
  { tag_specifications
  ; availability_zone
  ; availability_zone_id
  ; cidr_block
  ; ipv6_cidr_block
  ; outpost_arn
  ; vpc_id
  ; ipv6_native
  ; ipv4_ipam_pool_id
  ; ipv4_netmask_length
  ; ipv6_ipam_pool_id
  ; ipv6_netmask_length
  ; dry_run
  }

let parse xml =
  Some
    { tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; cidr_block = Aws.Util.option_bind (Aws.Xml.member "CidrBlock" xml) String.parse
    ; ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6CidrBlock" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "OutpostArn" xml) String.parse
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
    ; ipv6_native = Aws.Util.option_bind (Aws.Xml.member "Ipv6Native" xml) Boolean.parse
    ; ipv4_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "Ipv4IpamPoolId" xml) String.parse
    ; ipv4_netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "Ipv4NetmaskLength" xml) Integer.parse
    ; ipv6_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6IpamPoolId" xml) String.parse
    ; ipv6_netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6NetmaskLength" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.ipv6_netmask_length (fun f ->
             Aws.Query.Pair ("Ipv6NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.ipv6_ipam_pool_id (fun f ->
             Aws.Query.Pair ("Ipv6IpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.ipv4_netmask_length (fun f ->
             Aws.Query.Pair ("Ipv4NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.ipv4_ipam_pool_id (fun f ->
             Aws.Query.Pair ("Ipv4IpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.ipv6_native (fun f ->
             Aws.Query.Pair ("Ipv6Native", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.ipv6_netmask_length (fun f ->
             "Ipv6NetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.ipv6_ipam_pool_id (fun f ->
             "Ipv6IpamPoolId", String.to_json f)
       ; Aws.Util.option_map v.ipv4_netmask_length (fun f ->
             "Ipv4NetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.ipv4_ipam_pool_id (fun f ->
             "Ipv4IpamPoolId", String.to_json f)
       ; Aws.Util.option_map v.ipv6_native (fun f -> "Ipv6Native", Boolean.to_json f)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "OutpostArn", String.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             "Ipv6CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ])

let of_json j =
  { tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; cidr_block = Aws.Util.option_map (Aws.Json.lookup j "CidrBlock") String.of_json
  ; ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6CidrBlock") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "OutpostArn") String.of_json
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; ipv6_native = Aws.Util.option_map (Aws.Json.lookup j "Ipv6Native") Boolean.of_json
  ; ipv4_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv4IpamPoolId") String.of_json
  ; ipv4_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv4NetmaskLength") Integer.of_json
  ; ipv6_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6IpamPoolId") String.of_json
  ; ipv6_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6NetmaskLength") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
