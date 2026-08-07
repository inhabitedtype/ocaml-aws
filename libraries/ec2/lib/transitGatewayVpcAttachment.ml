open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t option
  ; transit_gateway_id : String.t option
  ; vpc_id : String.t option
  ; vpc_owner_id : String.t option
  ; state : TransitGatewayAttachmentState.t option
  ; subnet_ids : ValueStringList.t
  ; creation_time : DateTime.t option
  ; options : TransitGatewayVpcAttachmentOptions.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_attachment_id
    ?transit_gateway_id
    ?vpc_id
    ?vpc_owner_id
    ?state
    ?(subnet_ids = [])
    ?creation_time
    ?options
    ?(tags = [])
    () =
  { transit_gateway_attachment_id
  ; transit_gateway_id
  ; vpc_id
  ; vpc_owner_id
  ; state
  ; subnet_ids
  ; creation_time
  ; options
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; vpc_owner_id = Aws.Util.option_bind (Aws.Xml.member "vpcOwnerId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayAttachmentState.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "subnetIds" xml) ValueStringList.parse)
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "options" xml)
          TransitGatewayVpcAttachmentOptions.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair ("Options", TransitGatewayVpcAttachmentOptions.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("SubnetIds", ValueStringList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayAttachmentState.to_query f))
       ; Aws.Util.option_map v.vpc_owner_id (fun f ->
             Aws.Query.Pair ("VpcOwnerId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.options (fun f ->
             "options", TransitGatewayVpcAttachmentOptions.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Some ("subnetIds", ValueStringList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayAttachmentState.to_json f)
       ; Aws.Util.option_map v.vpc_owner_id (fun f -> "vpcOwnerId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; vpc_owner_id = Aws.Util.option_map (Aws.Json.lookup j "vpcOwnerId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayAttachmentState.of_json
  ; subnet_ids =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetIds"))
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "options")
        TransitGatewayVpcAttachmentOptions.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
