open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t
  ; transit_gateway_address : String.t option
  ; peer_address : String.t
  ; bgp_options : TransitGatewayConnectRequestBgpOptions.t option
  ; inside_cidr_blocks : InsideCidrBlocksStringList.t
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_attachment_id
    ?transit_gateway_address
    ~peer_address
    ?bgp_options
    ~inside_cidr_blocks
    ?(tag_specifications = [])
    ?dry_run
    () =
  { transit_gateway_attachment_id
  ; transit_gateway_address
  ; peer_address
  ; bgp_options
  ; inside_cidr_blocks
  ; tag_specifications
  ; dry_run
  }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Xml.required
          "TransitGatewayAttachmentId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayAttachmentId" xml)
             String.parse)
    ; transit_gateway_address =
        Aws.Util.option_bind (Aws.Xml.member "TransitGatewayAddress" xml) String.parse
    ; peer_address =
        Aws.Xml.required
          "PeerAddress"
          (Aws.Util.option_bind (Aws.Xml.member "PeerAddress" xml) String.parse)
    ; bgp_options =
        Aws.Util.option_bind
          (Aws.Xml.member "BgpOptions" xml)
          TransitGatewayConnectRequestBgpOptions.parse
    ; inside_cidr_blocks =
        Aws.Xml.required
          "InsideCidrBlocks"
          (Aws.Util.option_bind
             (Aws.Xml.member "InsideCidrBlocks" xml)
             InsideCidrBlocksStringList.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "InsideCidrBlocks"
              , InsideCidrBlocksStringList.to_query v.inside_cidr_blocks ))
       ; Aws.Util.option_map v.bgp_options (fun f ->
             Aws.Query.Pair
               ("BgpOptions", TransitGatewayConnectRequestBgpOptions.to_query f))
       ; Some (Aws.Query.Pair ("PeerAddress", String.to_query v.peer_address))
       ; Aws.Util.option_map v.transit_gateway_address (fun f ->
             Aws.Query.Pair ("TransitGatewayAddress", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayAttachmentId"
              , String.to_query v.transit_gateway_attachment_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("InsideCidrBlocks", InsideCidrBlocksStringList.to_json v.inside_cidr_blocks)
       ; Aws.Util.option_map v.bgp_options (fun f ->
             "BgpOptions", TransitGatewayConnectRequestBgpOptions.to_json f)
       ; Some ("PeerAddress", String.to_json v.peer_address)
       ; Aws.Util.option_map v.transit_gateway_address (fun f ->
             "TransitGatewayAddress", String.to_json f)
       ; Some
           ("TransitGatewayAttachmentId", String.to_json v.transit_gateway_attachment_id)
       ])

let of_json j =
  { transit_gateway_attachment_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayAttachmentId"))
  ; transit_gateway_address =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayAddress") String.of_json
  ; peer_address =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeerAddress"))
  ; bgp_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "BgpOptions")
        TransitGatewayConnectRequestBgpOptions.of_json
  ; inside_cidr_blocks =
      InsideCidrBlocksStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InsideCidrBlocks"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
