open Aws.BaseTypes

type t =
  { transit_gateway_id : String.t
  ; peer_transit_gateway_id : String.t
  ; peer_account_id : String.t
  ; peer_region : String.t
  ; options : CreateTransitGatewayPeeringAttachmentRequestOptions.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_id
    ~peer_transit_gateway_id
    ~peer_account_id
    ~peer_region
    ?options
    ?(tag_specifications = [])
    ?dry_run
    () =
  { transit_gateway_id
  ; peer_transit_gateway_id
  ; peer_account_id
  ; peer_region
  ; options
  ; tag_specifications
  ; dry_run
  }

let parse xml =
  Some
    { transit_gateway_id =
        Aws.Xml.required
          "TransitGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse)
    ; peer_transit_gateway_id =
        Aws.Xml.required
          "PeerTransitGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "PeerTransitGatewayId" xml) String.parse)
    ; peer_account_id =
        Aws.Xml.required
          "PeerAccountId"
          (Aws.Util.option_bind (Aws.Xml.member "PeerAccountId" xml) String.parse)
    ; peer_region =
        Aws.Xml.required
          "PeerRegion"
          (Aws.Util.option_bind (Aws.Xml.member "PeerRegion" xml) String.parse)
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "Options" xml)
          CreateTransitGatewayPeeringAttachmentRequestOptions.parse
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
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair
               ("Options", CreateTransitGatewayPeeringAttachmentRequestOptions.to_query f))
       ; Some (Aws.Query.Pair ("PeerRegion", String.to_query v.peer_region))
       ; Some (Aws.Query.Pair ("PeerAccountId", String.to_query v.peer_account_id))
       ; Some
           (Aws.Query.Pair
              ("PeerTransitGatewayId", String.to_query v.peer_transit_gateway_id))
       ; Some (Aws.Query.Pair ("TransitGatewayId", String.to_query v.transit_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.options (fun f ->
             "Options", CreateTransitGatewayPeeringAttachmentRequestOptions.to_json f)
       ; Some ("PeerRegion", String.to_json v.peer_region)
       ; Some ("PeerAccountId", String.to_json v.peer_account_id)
       ; Some ("PeerTransitGatewayId", String.to_json v.peer_transit_gateway_id)
       ; Some ("TransitGatewayId", String.to_json v.transit_gateway_id)
       ])

let of_json j =
  { transit_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayId"))
  ; peer_transit_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeerTransitGatewayId"))
  ; peer_account_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeerAccountId"))
  ; peer_region = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeerRegion"))
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "Options")
        CreateTransitGatewayPeeringAttachmentRequestOptions.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
