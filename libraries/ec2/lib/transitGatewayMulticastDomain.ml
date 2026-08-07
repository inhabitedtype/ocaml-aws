open Aws.BaseTypes

type t =
  { transit_gateway_multicast_domain_id : String.t option
  ; transit_gateway_id : String.t option
  ; transit_gateway_multicast_domain_arn : String.t option
  ; owner_id : String.t option
  ; options : TransitGatewayMulticastDomainOptions.t option
  ; state : TransitGatewayMulticastDomainState.t option
  ; creation_time : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_multicast_domain_id
    ?transit_gateway_id
    ?transit_gateway_multicast_domain_arn
    ?owner_id
    ?options
    ?state
    ?creation_time
    ?(tags = [])
    () =
  { transit_gateway_multicast_domain_id
  ; transit_gateway_id
  ; transit_gateway_multicast_domain_arn
  ; owner_id
  ; options
  ; state
  ; creation_time
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_multicast_domain_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMulticastDomainId" xml)
          String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; transit_gateway_multicast_domain_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMulticastDomainArn" xml)
          String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "options" xml)
          TransitGatewayMulticastDomainOptions.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayMulticastDomainState.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayMulticastDomainState.to_query f))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair ("Options", TransitGatewayMulticastDomainOptions.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_arn (fun f ->
             Aws.Query.Pair ("TransitGatewayMulticastDomainArn", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             Aws.Query.Pair ("TransitGatewayMulticastDomainId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayMulticastDomainState.to_json f)
       ; Aws.Util.option_map v.options (fun f ->
             "options", TransitGatewayMulticastDomainOptions.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_arn (fun f ->
             "transitGatewayMulticastDomainArn", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             "transitGatewayMulticastDomainId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_multicast_domain_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMulticastDomainId")
        String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; transit_gateway_multicast_domain_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMulticastDomainArn")
        String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "options")
        TransitGatewayMulticastDomainOptions.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayMulticastDomainState.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
