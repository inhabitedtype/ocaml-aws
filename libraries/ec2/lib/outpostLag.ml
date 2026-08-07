open Aws.BaseTypes

type t =
  { outpost_arn : String.t option
  ; owner_id : String.t option
  ; state : String.t option
  ; outpost_lag_id : String.t option
  ; local_gateway_virtual_interface_ids : LocalGatewayVirtualInterfaceIdSet.t
  ; service_link_virtual_interface_ids : ServiceLinkVirtualInterfaceIdSet.t
  ; tags : TagList.t
  }

let make
    ?outpost_arn
    ?owner_id
    ?state
    ?outpost_lag_id
    ?(local_gateway_virtual_interface_ids = [])
    ?(service_link_virtual_interface_ids = [])
    ?(tags = [])
    () =
  { outpost_arn
  ; owner_id
  ; state
  ; outpost_lag_id
  ; local_gateway_virtual_interface_ids
  ; service_link_virtual_interface_ids
  ; tags
  }

let parse xml =
  Some
    { outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; outpost_lag_id =
        Aws.Util.option_bind (Aws.Xml.member "outpostLagId" xml) String.parse
    ; local_gateway_virtual_interface_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "localGatewayVirtualInterfaceIdSet" xml)
             LocalGatewayVirtualInterfaceIdSet.parse)
    ; service_link_virtual_interface_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "serviceLinkVirtualInterfaceIdSet" xml)
             ServiceLinkVirtualInterfaceIdSet.parse)
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
              ( "ServiceLinkVirtualInterfaceIdSet"
              , ServiceLinkVirtualInterfaceIdSet.to_query
                  v.service_link_virtual_interface_ids ))
       ; Some
           (Aws.Query.Pair
              ( "LocalGatewayVirtualInterfaceIdSet"
              , LocalGatewayVirtualInterfaceIdSet.to_query
                  v.local_gateway_virtual_interface_ids ))
       ; Aws.Util.option_map v.outpost_lag_id (fun f ->
             Aws.Query.Pair ("OutpostLagId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Some
           ( "serviceLinkVirtualInterfaceIdSet"
           , ServiceLinkVirtualInterfaceIdSet.to_json v.service_link_virtual_interface_ids
           )
       ; Some
           ( "localGatewayVirtualInterfaceIdSet"
           , LocalGatewayVirtualInterfaceIdSet.to_json
               v.local_gateway_virtual_interface_ids )
       ; Aws.Util.option_map v.outpost_lag_id (fun f -> "outpostLagId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ])

let of_json j =
  { outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; outpost_lag_id = Aws.Util.option_map (Aws.Json.lookup j "outpostLagId") String.of_json
  ; local_gateway_virtual_interface_ids =
      LocalGatewayVirtualInterfaceIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "localGatewayVirtualInterfaceIdSet"))
  ; service_link_virtual_interface_ids =
      ServiceLinkVirtualInterfaceIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "serviceLinkVirtualInterfaceIdSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
