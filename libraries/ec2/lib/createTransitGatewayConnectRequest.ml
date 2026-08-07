open Aws.BaseTypes

type t =
  { transport_transit_gateway_attachment_id : String.t
  ; options : CreateTransitGatewayConnectRequestOptions.t
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ~transport_transit_gateway_attachment_id
    ~options
    ?(tag_specifications = [])
    ?dry_run
    () =
  { transport_transit_gateway_attachment_id; options; tag_specifications; dry_run }

let parse xml =
  Some
    { transport_transit_gateway_attachment_id =
        Aws.Xml.required
          "TransportTransitGatewayAttachmentId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransportTransitGatewayAttachmentId" xml)
             String.parse)
    ; options =
        Aws.Xml.required
          "Options"
          (Aws.Util.option_bind
             (Aws.Xml.member "Options" xml)
             CreateTransitGatewayConnectRequestOptions.parse)
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
              ("Options", CreateTransitGatewayConnectRequestOptions.to_query v.options))
       ; Some
           (Aws.Query.Pair
              ( "TransportTransitGatewayAttachmentId"
              , String.to_query v.transport_transit_gateway_attachment_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("Options", CreateTransitGatewayConnectRequestOptions.to_json v.options)
       ; Some
           ( "TransportTransitGatewayAttachmentId"
           , String.to_json v.transport_transit_gateway_attachment_id )
       ])

let of_json j =
  { transport_transit_gateway_attachment_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransportTransitGatewayAttachmentId"))
  ; options =
      CreateTransitGatewayConnectRequestOptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Options"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
