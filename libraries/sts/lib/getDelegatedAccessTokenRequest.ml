open Aws.BaseTypes

type t = { trade_in_token : String.t }

let make ~trade_in_token () = { trade_in_token }

let parse xml =
  Some
    { trade_in_token =
        Aws.Xml.required
          "TradeInToken"
          (Aws.Util.option_bind (Aws.Xml.member "TradeInToken" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TradeInToken", String.to_query v.trade_in_token)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("TradeInToken", String.to_json v.trade_in_token) ])

let of_json j =
  { trade_in_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TradeInToken"))
  }
