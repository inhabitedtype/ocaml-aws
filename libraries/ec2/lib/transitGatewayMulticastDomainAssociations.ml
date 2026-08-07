open Aws.BaseTypes

type t =
  { transit_gateway_multicast_domain_id : String.t option
  ; transit_gateway_attachment_id : String.t option
  ; resource_id : String.t option
  ; resource_type : TransitGatewayAttachmentResourceType.t option
  ; resource_owner_id : String.t option
  ; subnets : SubnetAssociationList.t
  }

let make
    ?transit_gateway_multicast_domain_id
    ?transit_gateway_attachment_id
    ?resource_id
    ?resource_type
    ?resource_owner_id
    ?(subnets = [])
    () =
  { transit_gateway_multicast_domain_id
  ; transit_gateway_attachment_id
  ; resource_id
  ; resource_type
  ; resource_owner_id
  ; subnets
  }

let parse xml =
  Some
    { transit_gateway_multicast_domain_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMulticastDomainId" xml)
          String.parse
    ; transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; resource_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwnerId" xml) String.parse
    ; subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subnets" xml)
             SubnetAssociationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Subnets", SubnetAssociationList.to_query v.subnets))
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             Aws.Query.Pair ("ResourceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair
               ("ResourceType", TransitGatewayAttachmentResourceType.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             Aws.Query.Pair ("TransitGatewayMulticastDomainId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("subnets", SubnetAssociationList.to_json v.subnets)
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             "resourceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", TransitGatewayAttachmentResourceType.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             "transitGatewayMulticastDomainId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_multicast_domain_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMulticastDomainId")
        String.of_json
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        TransitGatewayAttachmentResourceType.of_json
  ; resource_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwnerId") String.of_json
  ; subnets =
      SubnetAssociationList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnets"))
  }
