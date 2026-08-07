open Aws.BaseTypes

type t =
  { client_vpn_target_networks : TargetNetworkSet.t
  ; next_token : String.t option
  }

let make ?(client_vpn_target_networks = []) ?next_token () =
  { client_vpn_target_networks; next_token }

let parse xml =
  Some
    { client_vpn_target_networks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "clientVpnTargetNetworks" xml)
             TargetNetworkSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ClientVpnTargetNetworks"
              , TargetNetworkSet.to_query v.client_vpn_target_networks ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "clientVpnTargetNetworks"
           , TargetNetworkSet.to_json v.client_vpn_target_networks )
       ])

let of_json j =
  { client_vpn_target_networks =
      TargetNetworkSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "clientVpnTargetNetworks"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
