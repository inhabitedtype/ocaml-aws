open Aws.BaseTypes

type t =
  { transit_gateway_route_table_id : String.t
  ; prefix_list_id : String.t
  ; transit_gateway_attachment_id : String.t option
  ; blackhole : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_route_table_id
    ~prefix_list_id
    ?transit_gateway_attachment_id
    ?blackhole
    ?dry_run
    () =
  { transit_gateway_route_table_id
  ; prefix_list_id
  ; transit_gateway_attachment_id
  ; blackhole
  ; dry_run
  }

let parse xml =
  Some
    { transit_gateway_route_table_id =
        Aws.Xml.required
          "TransitGatewayRouteTableId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayRouteTableId" xml)
             String.parse)
    ; prefix_list_id =
        Aws.Xml.required
          "PrefixListId"
          (Aws.Util.option_bind (Aws.Xml.member "PrefixListId" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("PrefixListId", String.to_query v.prefix_list_id))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayRouteTableId"
              , String.to_query v.transit_gateway_route_table_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.blackhole (fun f -> "Blackhole", Boolean.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "TransitGatewayAttachmentId", String.to_json f)
       ; Some ("PrefixListId", String.to_json v.prefix_list_id)
       ; Some
           ("TransitGatewayRouteTableId", String.to_json v.transit_gateway_route_table_id)
       ])

let of_json j =
  { transit_gateway_route_table_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayRouteTableId"))
  ; prefix_list_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrefixListId"))
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayAttachmentId") String.of_json
  ; blackhole = Aws.Util.option_map (Aws.Json.lookup j "Blackhole") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
