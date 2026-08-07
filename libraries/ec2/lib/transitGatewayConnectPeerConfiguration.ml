open Aws.BaseTypes

type t =
  { transit_gateway_address : String.t option
  ; peer_address : String.t option
  ; inside_cidr_blocks : InsideCidrBlocksStringList.t
  ; protocol : ProtocolValue.t option
  ; bgp_configurations : TransitGatewayAttachmentBgpConfigurationList.t
  }

let make
    ?transit_gateway_address
    ?peer_address
    ?(inside_cidr_blocks = [])
    ?protocol
    ?(bgp_configurations = [])
    () =
  { transit_gateway_address
  ; peer_address
  ; inside_cidr_blocks
  ; protocol
  ; bgp_configurations
  }

let parse xml =
  Some
    { transit_gateway_address =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayAddress" xml) String.parse
    ; peer_address = Aws.Util.option_bind (Aws.Xml.member "peerAddress" xml) String.parse
    ; inside_cidr_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "insideCidrBlocks" xml)
             InsideCidrBlocksStringList.parse)
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) ProtocolValue.parse
    ; bgp_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "bgpConfigurations" xml)
             TransitGatewayAttachmentBgpConfigurationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "BgpConfigurations"
              , TransitGatewayAttachmentBgpConfigurationList.to_query v.bgp_configurations
              ))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", ProtocolValue.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InsideCidrBlocks"
              , InsideCidrBlocksStringList.to_query v.inside_cidr_blocks ))
       ; Aws.Util.option_map v.peer_address (fun f ->
             Aws.Query.Pair ("PeerAddress", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_address (fun f ->
             Aws.Query.Pair ("TransitGatewayAddress", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "bgpConfigurations"
           , TransitGatewayAttachmentBgpConfigurationList.to_json v.bgp_configurations )
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", ProtocolValue.to_json f)
       ; Some ("insideCidrBlocks", InsideCidrBlocksStringList.to_json v.inside_cidr_blocks)
       ; Aws.Util.option_map v.peer_address (fun f -> "peerAddress", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_address (fun f ->
             "transitGatewayAddress", String.to_json f)
       ])

let of_json j =
  { transit_gateway_address =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAddress") String.of_json
  ; peer_address = Aws.Util.option_map (Aws.Json.lookup j "peerAddress") String.of_json
  ; inside_cidr_blocks =
      InsideCidrBlocksStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "insideCidrBlocks"))
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") ProtocolValue.of_json
  ; bgp_configurations =
      TransitGatewayAttachmentBgpConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "bgpConfigurations"))
  }
