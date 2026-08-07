open Aws.BaseTypes

type t =
  { destination_cidr_block : String.t
  ; transit_gateway_route_table_id : String.t
  ; transit_gateway_attachment_id : String.t option
  ; blackhole : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make
    ~destination_cidr_block
    ~transit_gateway_route_table_id
    ?transit_gateway_attachment_id
    ?blackhole
    ?dry_run
    () =
  { destination_cidr_block
  ; transit_gateway_route_table_id
  ; transit_gateway_attachment_id
  ; blackhole
  ; dry_run
  }

let parse xml =
  Some
    { destination_cidr_block =
        Aws.Xml.required
          "DestinationCidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse)
    ; transit_gateway_route_table_id =
        Aws.Xml.required
          "TransitGatewayRouteTableId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayRouteTableId" xml)
             String.parse)
    ; transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitGatewayAttachmentId" xml)
          String.parse
    ; blackhole = Aws.Util.option_bind (Aws.Xml.member "Blackhole" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.blackhole (fun f ->
             Aws.Query.Pair ("Blackhole", Boolean.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayRouteTableId"
              , String.to_query v.transit_gateway_route_table_id ))
       ; Some
           (Aws.Query.Pair
              ("DestinationCidrBlock", String.to_query v.destination_cidr_block))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.blackhole (fun f -> "Blackhole", Boolean.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "TransitGatewayAttachmentId", String.to_json f)
       ; Some
           ("TransitGatewayRouteTableId", String.to_json v.transit_gateway_route_table_id)
       ; Some ("DestinationCidrBlock", String.to_json v.destination_cidr_block)
       ])

let of_json j =
  { destination_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationCidrBlock"))
  ; transit_gateway_route_table_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayRouteTableId"))
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayAttachmentId") String.of_json
  ; blackhole = Aws.Util.option_map (Aws.Json.lookup j "Blackhole") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
