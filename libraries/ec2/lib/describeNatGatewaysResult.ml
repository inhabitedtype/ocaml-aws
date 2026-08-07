open Aws.BaseTypes

type t =
  { nat_gateways : NatGatewayList.t
  ; next_token : String.t option
  }

let make ?(nat_gateways = []) ?next_token () = { nat_gateways; next_token }

let parse xml =
  Some
    { nat_gateways =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "natGatewaySet" xml) NatGatewayList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("NatGatewaySet", NatGatewayList.to_query v.nat_gateways))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("natGatewaySet", NatGatewayList.to_json v.nat_gateways)
       ])

let of_json j =
  { nat_gateways =
      NatGatewayList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "natGatewaySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
