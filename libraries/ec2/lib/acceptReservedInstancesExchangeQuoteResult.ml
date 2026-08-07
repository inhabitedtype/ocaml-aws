open Aws.BaseTypes

type t = { exchange_id : String.t option }

let make ?exchange_id () = { exchange_id }

let parse xml =
  Some
    { exchange_id = Aws.Util.option_bind (Aws.Xml.member "exchangeId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.exchange_id (fun f ->
             Aws.Query.Pair ("ExchangeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.exchange_id (fun f -> "exchangeId", String.to_json f) ])

let of_json j =
  { exchange_id = Aws.Util.option_map (Aws.Json.lookup j "exchangeId") String.of_json }
