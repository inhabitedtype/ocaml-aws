open Aws.BaseTypes

type t =
  { term : Long.t option
  ; price : Double.t option
  ; currency_code : CurrencyCodeValues.t option
  }

let make ?term ?price ?currency_code () = { term; price; currency_code }

let parse xml =
  Some
    { term = Aws.Util.option_bind (Aws.Xml.member "term" xml) Long.parse
    ; price = Aws.Util.option_bind (Aws.Xml.member "price" xml) Double.parse
    ; currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ; Aws.Util.option_map v.price (fun f ->
             Aws.Query.Pair ("Price", Double.to_query f))
       ; Aws.Util.option_map v.term (fun f -> Aws.Query.Pair ("Term", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.currency_code (fun f ->
             "currencyCode", CurrencyCodeValues.to_json f)
       ; Aws.Util.option_map v.price (fun f -> "price", Double.to_json f)
       ; Aws.Util.option_map v.term (fun f -> "term", Long.to_json f)
       ])

let of_json j =
  { term = Aws.Util.option_map (Aws.Json.lookup j "term") Long.of_json
  ; price = Aws.Util.option_map (Aws.Json.lookup j "price") Double.of_json
  ; currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "currencyCode") CurrencyCodeValues.of_json
  }
