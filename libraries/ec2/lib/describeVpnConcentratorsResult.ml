open Aws.BaseTypes

type t =
  { vpn_concentrators : VpnConcentratorList.t
  ; next_token : String.t option
  }

let make ?(vpn_concentrators = []) ?next_token () = { vpn_concentrators; next_token }

let parse xml =
  Some
    { vpn_concentrators =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpnConcentratorSet" xml)
             VpnConcentratorList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VpnConcentratorSet", VpnConcentratorList.to_query v.vpn_concentrators))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("vpnConcentratorSet", VpnConcentratorList.to_json v.vpn_concentrators)
       ])

let of_json j =
  { vpn_concentrators =
      VpnConcentratorList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "vpnConcentratorSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
