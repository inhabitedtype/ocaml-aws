open Aws.BaseTypes

type t =
  { amount : Double.t option
  ; currency_code : CurrencyCodeValues.t option
  }

let make ?amount ?currency_code () = { amount; currency_code }

let parse xml =
  Some
    { amount = Aws.Util.option_bind (Aws.Xml.member "amount" xml) Double.parse
    ; currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ; Aws.Util.option_map v.amount (fun f ->
             Aws.Query.Pair ("Amount", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.currency_code (fun f ->
             "currencyCode", CurrencyCodeValues.to_json f)
       ; Aws.Util.option_map v.amount (fun f -> "amount", Double.to_json f)
       ])

let of_json j =
  { amount = Aws.Util.option_map (Aws.Json.lookup j "amount") Double.of_json
  ; currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "currencyCode") CurrencyCodeValues.of_json
  }
