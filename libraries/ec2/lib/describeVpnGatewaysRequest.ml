open Aws.BaseTypes

type t =
  { filters : FilterList.t
  ; vpn_gateway_ids : VpnGatewayIdStringList.t
  ; dry_run : Boolean.t option
  }

let make ?(filters = []) ?(vpn_gateway_ids = []) ?dry_run () =
  { filters; vpn_gateway_ids; dry_run }

let parse xml =
  Some
    { filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; vpn_gateway_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpnGatewayId" xml)
             VpnGatewayIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VpnGatewayId", VpnGatewayIdStringList.to_query v.vpn_gateway_ids))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("VpnGatewayId", VpnGatewayIdStringList.to_json v.vpn_gateway_ids)
       ; Some ("Filter", FilterList.to_json v.filters)
       ])

let of_json j =
  { filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; vpn_gateway_ids =
      VpnGatewayIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnGatewayId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
