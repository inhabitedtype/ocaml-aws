open Aws.BaseTypes

type t =
  { group_ip_address : String.t option
  ; transit_gateway_attachment_id : String.t option
  ; subnet_id : String.t option
  ; resource_id : String.t option
  ; resource_type : TransitGatewayAttachmentResourceType.t option
  ; resource_owner_id : String.t option
  ; network_interface_id : String.t option
  ; group_member : Boolean.t option
  ; group_source : Boolean.t option
  ; member_type : MembershipType.t option
  ; source_type : MembershipType.t option
  }

let make
    ?group_ip_address
    ?transit_gateway_attachment_id
    ?subnet_id
    ?resource_id
    ?resource_type
    ?resource_owner_id
    ?network_interface_id
    ?group_member
    ?group_source
    ?member_type
    ?source_type
    () =
  { group_ip_address
  ; transit_gateway_attachment_id
  ; subnet_id
  ; resource_id
  ; resource_type
  ; resource_owner_id
  ; network_interface_id
  ; group_member
  ; group_source
  ; member_type
  ; source_type
  }

let parse xml =
  Some
    { group_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "groupIpAddress" xml) String.parse
    ; transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; resource_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwnerId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; group_member = Aws.Util.option_bind (Aws.Xml.member "groupMember" xml) Boolean.parse
    ; group_source = Aws.Util.option_bind (Aws.Xml.member "groupSource" xml) Boolean.parse
    ; member_type =
        Aws.Util.option_bind (Aws.Xml.member "memberType" xml) MembershipType.parse
    ; source_type =
        Aws.Util.option_bind (Aws.Xml.member "sourceType" xml) MembershipType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", MembershipType.to_query f))
       ; Aws.Util.option_map v.member_type (fun f ->
             Aws.Query.Pair ("MemberType", MembershipType.to_query f))
       ; Aws.Util.option_map v.group_source (fun f ->
             Aws.Query.Pair ("GroupSource", Boolean.to_query f))
       ; Aws.Util.option_map v.group_member (fun f ->
             Aws.Query.Pair ("GroupMember", Boolean.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             Aws.Query.Pair ("ResourceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair
               ("ResourceType", TransitGatewayAttachmentResourceType.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.group_ip_address (fun f ->
             Aws.Query.Pair ("GroupIpAddress", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_type (fun f ->
             "sourceType", MembershipType.to_json f)
       ; Aws.Util.option_map v.member_type (fun f ->
             "memberType", MembershipType.to_json f)
       ; Aws.Util.option_map v.group_source (fun f -> "groupSource", Boolean.to_json f)
       ; Aws.Util.option_map v.group_member (fun f -> "groupMember", Boolean.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             "resourceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", TransitGatewayAttachmentResourceType.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.group_ip_address (fun f ->
             "groupIpAddress", String.to_json f)
       ])

let of_json j =
  { group_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "groupIpAddress") String.of_json
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        TransitGatewayAttachmentResourceType.of_json
  ; resource_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwnerId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; group_member = Aws.Util.option_map (Aws.Json.lookup j "groupMember") Boolean.of_json
  ; group_source = Aws.Util.option_map (Aws.Json.lookup j "groupSource") Boolean.of_json
  ; member_type =
      Aws.Util.option_map (Aws.Json.lookup j "memberType") MembershipType.of_json
  ; source_type =
      Aws.Util.option_map (Aws.Json.lookup j "sourceType") MembershipType.of_json
  }
