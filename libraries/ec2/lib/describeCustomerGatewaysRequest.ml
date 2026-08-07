open Aws.BaseTypes

type t =
  { customer_gateway_ids : CustomerGatewayIdStringList.t
  ; filters : FilterList.t
  ; dry_run : Boolean.t option
  }

let make ?(customer_gateway_ids = []) ?(filters = []) ?dry_run () =
  { customer_gateway_ids; filters; dry_run }

let parse xml =
  Some
    { customer_gateway_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CustomerGatewayId" xml)
             CustomerGatewayIdStringList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ( "CustomerGatewayId"
              , CustomerGatewayIdStringList.to_query v.customer_gateway_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some
           ( "CustomerGatewayId"
           , CustomerGatewayIdStringList.to_json v.customer_gateway_ids )
       ])

let of_json j =
  { customer_gateway_ids =
      CustomerGatewayIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomerGatewayId"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
