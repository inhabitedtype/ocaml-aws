open Aws.BaseTypes

type t =
  { nat_gateway_id : String.t option
  ; nat_gateway_addresses : NatGatewayAddressList.t
  }

let make ?nat_gateway_id ?(nat_gateway_addresses = []) () =
  { nat_gateway_id; nat_gateway_addresses }

let parse xml =
  Some
    { nat_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "natGatewayId" xml) String.parse
    ; nat_gateway_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "natGatewayAddressSet" xml)
             NatGatewayAddressList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "NatGatewayAddressSet"
              , NatGatewayAddressList.to_query v.nat_gateway_addresses ))
       ; Aws.Util.option_map v.nat_gateway_id (fun f ->
             Aws.Query.Pair ("NatGatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("natGatewayAddressSet", NatGatewayAddressList.to_json v.nat_gateway_addresses)
       ; Aws.Util.option_map v.nat_gateway_id (fun f -> "natGatewayId", String.to_json f)
       ])

let of_json j =
  { nat_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "natGatewayId") String.of_json
  ; nat_gateway_addresses =
      NatGatewayAddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "natGatewayAddressSet"))
  }
