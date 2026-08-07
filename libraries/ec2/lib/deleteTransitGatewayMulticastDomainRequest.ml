open Aws.BaseTypes

type t =
  { transit_gateway_multicast_domain_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~transit_gateway_multicast_domain_id ?dry_run () =
  { transit_gateway_multicast_domain_id; dry_run }

let parse xml =
  Some
    { transit_gateway_multicast_domain_id =
        Aws.Xml.required
          "TransitGatewayMulticastDomainId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayMulticastDomainId" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayMulticastDomainId"
              , String.to_query v.transit_gateway_multicast_domain_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "TransitGatewayMulticastDomainId"
           , String.to_json v.transit_gateway_multicast_domain_id )
       ])

let of_json j =
  { transit_gateway_multicast_domain_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayMulticastDomainId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
