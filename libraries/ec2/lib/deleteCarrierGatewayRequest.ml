open Aws.BaseTypes

type t =
  { carrier_gateway_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~carrier_gateway_id ?dry_run () = { carrier_gateway_id; dry_run }

let parse xml =
  Some
    { carrier_gateway_id =
        Aws.Xml.required
          "CarrierGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "CarrierGatewayId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("CarrierGatewayId", String.to_query v.carrier_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("CarrierGatewayId", String.to_json v.carrier_gateway_id)
       ])

let of_json j =
  { carrier_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CarrierGatewayId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
