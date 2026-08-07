open Aws.BaseTypes

type t = { gateway_id : String.t option }

let make ?gateway_id () = { gateway_id }

let parse xml =
  Some { gateway_id = Aws.Util.option_bind (Aws.Xml.member "gatewayId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.gateway_id (fun f ->
             Aws.Query.Pair ("GatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.gateway_id (fun f -> "gatewayId", String.to_json f) ])

let of_json j =
  { gateway_id = Aws.Util.option_map (Aws.Json.lookup j "gatewayId") String.of_json }
