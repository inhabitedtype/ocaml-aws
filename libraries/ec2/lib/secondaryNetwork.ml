open Aws.BaseTypes

type t =
  { secondary_network_id : String.t option
  ; secondary_network_arn : String.t option
  ; owner_id : String.t option
  ; type_ : SecondaryNetworkType.t option
  ; state : SecondaryNetworkState.t option
  ; state_reason : String.t option
  ; ipv4_cidr_block_associations : SecondaryNetworkIpv4CidrBlockAssociationList.t
  ; tags : TagList.t
  }

let make
    ?secondary_network_id
    ?secondary_network_arn
    ?owner_id
    ?type_
    ?state
    ?state_reason
    ?(ipv4_cidr_block_associations = [])
    ?(tags = [])
    () =
  { secondary_network_id
  ; secondary_network_arn
  ; owner_id
  ; type_
  ; state
  ; state_reason
  ; ipv4_cidr_block_associations
  ; tags
  }

let parse xml =
  Some
    { secondary_network_id =
        Aws.Util.option_bind (Aws.Xml.member "secondaryNetworkId" xml) String.parse
    ; secondary_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "secondaryNetworkArn" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) SecondaryNetworkType.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) SecondaryNetworkState.parse
    ; state_reason = Aws.Util.option_bind (Aws.Xml.member "stateReason" xml) String.parse
    ; ipv4_cidr_block_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv4CidrBlockAssociationSet" xml)
             SecondaryNetworkIpv4CidrBlockAssociationList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "Ipv4CidrBlockAssociationSet"
              , SecondaryNetworkIpv4CidrBlockAssociationList.to_query
                  v.ipv4_cidr_block_associations ))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SecondaryNetworkState.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", SecondaryNetworkType.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.secondary_network_arn (fun f ->
             Aws.Query.Pair ("SecondaryNetworkArn", String.to_query f))
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             Aws.Query.Pair ("SecondaryNetworkId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Some
           ( "ipv4CidrBlockAssociationSet"
           , SecondaryNetworkIpv4CidrBlockAssociationList.to_json
               v.ipv4_cidr_block_associations )
       ; Aws.Util.option_map v.state_reason (fun f -> "stateReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", SecondaryNetworkState.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", SecondaryNetworkType.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.secondary_network_arn (fun f ->
             "secondaryNetworkArn", String.to_json f)
       ; Aws.Util.option_map v.secondary_network_id (fun f ->
             "secondaryNetworkId", String.to_json f)
       ])

let of_json j =
  { secondary_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryNetworkId") String.of_json
  ; secondary_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryNetworkArn") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") SecondaryNetworkType.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") SecondaryNetworkState.of_json
  ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "stateReason") String.of_json
  ; ipv4_cidr_block_associations =
      SecondaryNetworkIpv4CidrBlockAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv4CidrBlockAssociationSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
