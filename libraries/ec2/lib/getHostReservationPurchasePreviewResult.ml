open Aws.BaseTypes

type t =
  { currency_code : CurrencyCodeValues.t option
  ; purchase : PurchaseSet.t
  ; total_hourly_price : String.t option
  ; total_upfront_price : String.t option
  }

let make ?currency_code ?(purchase = []) ?total_hourly_price ?total_upfront_price () =
  { currency_code; purchase; total_hourly_price; total_upfront_price }

let parse xml =
  Some
    { currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    ; purchase =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "purchase" xml) PurchaseSet.parse)
    ; total_hourly_price =
        Aws.Util.option_bind (Aws.Xml.member "totalHourlyPrice" xml) String.parse
    ; total_upfront_price =
        Aws.Util.option_bind (Aws.Xml.member "totalUpfrontPrice" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_upfront_price (fun f ->
             Aws.Query.Pair ("TotalUpfrontPrice", String.to_query f))
       ; Aws.Util.option_map v.total_hourly_price (fun f ->
             Aws.Query.Pair ("TotalHourlyPrice", String.to_query f))
       ; Some (Aws.Query.Pair ("Purchase", PurchaseSet.to_query v.purchase))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_upfront_price (fun f ->
             "totalUpfrontPrice", String.to_json f)
       ; Aws.Util.option_map v.total_hourly_price (fun f ->
             "totalHourlyPrice", String.to_json f)
       ; Some ("purchase", PurchaseSet.to_json v.purchase)
       ; Aws.Util.option_map v.currency_code (fun f ->
             "currencyCode", CurrencyCodeValues.to_json f)
       ])

let of_json j =
  { currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "currencyCode") CurrencyCodeValues.of_json
  ; purchase = PurchaseSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "purchase"))
  ; total_hourly_price =
      Aws.Util.option_map (Aws.Json.lookup j "totalHourlyPrice") String.of_json
  ; total_upfront_price =
      Aws.Util.option_map (Aws.Json.lookup j "totalUpfrontPrice") String.of_json
  }
