open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; nat_gateway : NatGateway.t option
  }

let make ?client_token ?nat_gateway () = { client_token; nat_gateway }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; nat_gateway =
        Aws.Util.option_bind (Aws.Xml.member "natGateway" xml) NatGateway.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nat_gateway (fun f ->
             Aws.Query.Pair ("NatGateway", NatGateway.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nat_gateway (fun f -> "natGateway", NatGateway.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; nat_gateway = Aws.Util.option_map (Aws.Json.lookup j "natGateway") NatGateway.of_json
  }
