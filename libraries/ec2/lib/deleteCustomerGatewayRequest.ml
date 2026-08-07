open Aws.BaseTypes

type t =
  { customer_gateway_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~customer_gateway_id ?dry_run () = { customer_gateway_id; dry_run }

let parse xml =
  Some
    { customer_gateway_id =
        Aws.Xml.required
          "CustomerGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "CustomerGatewayId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("CustomerGatewayId", String.to_query v.customer_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("CustomerGatewayId", String.to_json v.customer_gateway_id)
       ])

let of_json j =
  { customer_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomerGatewayId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
