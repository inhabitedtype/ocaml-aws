open Aws.BaseTypes

type t =
  { active : Boolean.t option
  ; currency_code : CurrencyCodeValues.t option
  ; price : Double.t option
  ; term : Long.t option
  }

let make ?active ?currency_code ?price ?term () = { active; currency_code; price; term }

let parse xml =
  Some
    { active = Aws.Util.option_bind (Aws.Xml.member "active" xml) Boolean.parse
    ; currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    ; price = Aws.Util.option_bind (Aws.Xml.member "price" xml) Double.parse
    ; term = Aws.Util.option_bind (Aws.Xml.member "term" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.term (fun f -> Aws.Query.Pair ("Term", Long.to_query f))
       ; Aws.Util.option_map v.price (fun f ->
             Aws.Query.Pair ("Price", Double.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ; Aws.Util.option_map v.active (fun f ->
             Aws.Query.Pair ("Active", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.term (fun f -> "term", Long.to_json f)
       ; Aws.Util.option_map v.price (fun f -> "price", Double.to_json f)
       ; Aws.Util.option_map v.currency_code (fun f ->
             "currencyCode", CurrencyCodeValues.to_json f)
       ; Aws.Util.option_map v.active (fun f -> "active", Boolean.to_json f)
       ])

let of_json j =
  { active = Aws.Util.option_map (Aws.Json.lookup j "active") Boolean.of_json
  ; currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "currencyCode") CurrencyCodeValues.of_json
  ; price = Aws.Util.option_map (Aws.Json.lookup j "price") Double.of_json
  ; term = Aws.Util.option_map (Aws.Json.lookup j "term") Long.of_json
  }
