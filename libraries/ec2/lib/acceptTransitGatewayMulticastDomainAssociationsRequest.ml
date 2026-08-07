open Aws.BaseTypes

type t =
  { transit_gateway_multicast_domain_id : String.t option
  ; transit_gateway_attachment_id : String.t option
  ; subnet_ids : ValueStringList.t
  ; dry_run : Boolean.t option
  }

let make
    ?transit_gateway_multicast_domain_id
    ?transit_gateway_attachment_id
    ?(subnet_ids = [])
    ?dry_run
    () =
  { transit_gateway_multicast_domain_id
  ; transit_gateway_attachment_id
  ; subnet_ids
  ; dry_run
  }

let parse xml =
  Some
    { transit_gateway_multicast_domain_id =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitGatewayMulticastDomainId" xml)
          String.parse
    ; transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitGatewayAttachmentId" xml)
          String.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "SubnetIds" xml) ValueStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("SubnetIds", ValueStringList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             Aws.Query.Pair ("TransitGatewayMulticastDomainId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("SubnetIds", ValueStringList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "TransitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             "TransitGatewayMulticastDomainId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_multicast_domain_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitGatewayMulticastDomainId")
        String.of_json
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayAttachmentId") String.of_json
  ; subnet_ids =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
