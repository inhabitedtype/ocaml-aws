open Aws.BaseTypes

type t =
  { source_transit_gateway_attachment_id : String.t option
  ; source_transit_gateway_attachment_type : TransitGatewayAttachmentResourceType.t option
  ; source_cidr_block : String.t option
  ; source_port_range : String.t option
  ; destination_transit_gateway_attachment_id : String.t option
  ; destination_transit_gateway_attachment_type :
      TransitGatewayAttachmentResourceType.t option
  ; destination_cidr_block : String.t option
  ; destination_port_range : String.t option
  ; protocol : String.t option
  }

let make
    ?source_transit_gateway_attachment_id
    ?source_transit_gateway_attachment_type
    ?source_cidr_block
    ?source_port_range
    ?destination_transit_gateway_attachment_id
    ?destination_transit_gateway_attachment_type
    ?destination_cidr_block
    ?destination_port_range
    ?protocol
    () =
  { source_transit_gateway_attachment_id
  ; source_transit_gateway_attachment_type
  ; source_cidr_block
  ; source_port_range
  ; destination_transit_gateway_attachment_id
  ; destination_transit_gateway_attachment_type
  ; destination_cidr_block
  ; destination_port_range
  ; protocol
  }

let parse xml =
  Some
    { source_transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceTransitGatewayAttachmentId" xml)
          String.parse
    ; source_transit_gateway_attachment_type =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceTransitGatewayAttachmentType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; source_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "sourceCidrBlock" xml) String.parse
    ; source_port_range =
        Aws.Util.option_bind (Aws.Xml.member "sourcePortRange" xml) String.parse
    ; destination_transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "destinationTransitGatewayAttachmentId" xml)
          String.parse
    ; destination_transit_gateway_attachment_type =
        Aws.Util.option_bind
          (Aws.Xml.member "destinationTransitGatewayAttachmentType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; destination_port_range =
        Aws.Util.option_bind (Aws.Xml.member "destinationPortRange" xml) String.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair ("DestinationPortRange", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_type (fun f ->
             Aws.Query.Pair
               ( "DestinationTransitGatewayAttachmentType"
               , TransitGatewayAttachmentResourceType.to_query f ))
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("DestinationTransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", String.to_query f))
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             Aws.Query.Pair ("SourceCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.source_transit_gateway_attachment_type (fun f ->
             Aws.Query.Pair
               ( "SourceTransitGatewayAttachmentType"
               , TransitGatewayAttachmentResourceType.to_query f ))
       ; Aws.Util.option_map v.source_transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("SourceTransitGatewayAttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.protocol (fun f -> "protocol", String.to_json f)
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             "destinationPortRange", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_type (fun f ->
             ( "destinationTransitGatewayAttachmentType"
             , TransitGatewayAttachmentResourceType.to_json f ))
       ; Aws.Util.option_map v.destination_transit_gateway_attachment_id (fun f ->
             "destinationTransitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "sourcePortRange", String.to_json f)
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             "sourceCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.source_transit_gateway_attachment_type (fun f ->
             ( "sourceTransitGatewayAttachmentType"
             , TransitGatewayAttachmentResourceType.to_json f ))
       ; Aws.Util.option_map v.source_transit_gateway_attachment_id (fun f ->
             "sourceTransitGatewayAttachmentId", String.to_json f)
       ])

let of_json j =
  { source_transit_gateway_attachment_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourceTransitGatewayAttachmentId")
        String.of_json
  ; source_transit_gateway_attachment_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourceTransitGatewayAttachmentType")
        TransitGatewayAttachmentResourceType.of_json
  ; source_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "sourceCidrBlock") String.of_json
  ; source_port_range =
      Aws.Util.option_map (Aws.Json.lookup j "sourcePortRange") String.of_json
  ; destination_transit_gateway_attachment_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "destinationTransitGatewayAttachmentId")
        String.of_json
  ; destination_transit_gateway_attachment_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "destinationTransitGatewayAttachmentType")
        TransitGatewayAttachmentResourceType.of_json
  ; destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; destination_port_range =
      Aws.Util.option_map (Aws.Json.lookup j "destinationPortRange") String.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") String.of_json
  }
