open Aws.BaseTypes

type t =
  { secondary_subnet_id : String.t option
  ; secondary_subnet_arn : String.t option
  ; secondary_network_id : String.t option
  ; secondary_network_type : SecondaryNetworkType.t option
  ; owner_id : String.t option
  ; availability_zone_id : String.t option
  ; availability_zone : String.t option
  ; ipv4_cidr_block_associations : SecondarySubnetIpv4CidrBlockAssociationList.t
  ; state : SecondarySubnetState.t option
  ; state_reason : String.t option
  ; tags : TagList.t
  }

let make
    ?secondary_subnet_id
    ?secondary_subnet_arn
    ?secondary_network_id
    ?secondary_network_type
    ?owner_id
    ?availability_zone_id
    ?availability_zone
    ?(ipv4_cidr_block_associations = [])
    ?state
    ?state_reason
    ?(tags = [])
    () =
  { secondary_subnet_id
  ; secondary_subnet_arn
  ; secondary_network_id
  ; secondary_network_type
  ; owner_id
  ; availability_zone_id
  ; availability_zone
  ; ipv4_cidr_block_associations
  ; state
  ; state_reason
  ; tags
  }

let parse xml =
  Some
    { secondary_subnet_id =
        Aws.Util.option_bind (Aws.Xml.member "secondarySubnetId" xml) String.parse
    ; secondary_subnet_arn =
        Aws.Util.option_bind (Aws.Xml.member "secondarySubnetArn" xml) String.parse
    ; secondary_network_id =
        Aws.Util.option_bind (Aws.Xml.member "secondaryNetworkId" xml) String.parse
    ; secondary_network_type =
        Aws.Util.option_bind
          (Aws.Xml.member "secondaryNetworkType" xml)
          SecondaryNetworkType.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; ipv4_cidr_block_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv4CidrBlockAssociationSet" xml)
             SecondarySubnetIpv4CidrBlockAssociationList.parse)
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) SecondarySubnetState.parse
    ; state_reason = Aws.Util.option_bind (Aws.Xml.member "stateReason" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SecondarySubnetState.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "Ipv4CidrBlockAssociationSet"
              , SecondarySubnetIpv4CidrBlockAssociationList.to_query
                  v.ipv4_cidr_block_associations ))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.secondary_network_type (fun f ->
             Aws.Query.Pair ("SecondaryNetworkType", SecondaryNetworkType.to_query f))
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             Aws.Query.Pair ("SecondaryNetworkId", String.to_query f))
       ; Aws.Util.option_map v.secondary_subnet_arn (fun f ->
             Aws.Query.Pair ("SecondarySubnetArn", String.to_query f))
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             Aws.Query.Pair ("SecondarySubnetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state_reason (fun f -> "stateReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", SecondarySubnetState.to_json f)
       ; Some
           ( "ipv4CidrBlockAssociationSet"
           , SecondarySubnetIpv4CidrBlockAssociationList.to_json
               v.ipv4_cidr_block_associations )
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.secondary_network_type (fun f ->
             "secondaryNetworkType", SecondaryNetworkType.to_json f)
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             "secondaryNetworkId", String.to_json f)
       ; Aws.Util.option_map v.secondary_subnet_arn (fun f ->
             "secondarySubnetArn", String.to_json f)
       ; Aws.Util.option_map v.secondary_subnet_id (fun f ->
             "secondarySubnetId", String.to_json f)
       ])

let of_json j =
  { secondary_subnet_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondarySubnetId") String.of_json
  ; secondary_subnet_arn =
      Aws.Util.option_map (Aws.Json.lookup j "secondarySubnetArn") String.of_json
  ; secondary_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryNetworkId") String.of_json
  ; secondary_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "secondaryNetworkType")
        SecondaryNetworkType.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; ipv4_cidr_block_associations =
      SecondarySubnetIpv4CidrBlockAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv4CidrBlockAssociationSet"))
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") SecondarySubnetState.of_json
  ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "stateReason") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
