open Aws.BaseTypes

type t =
  { hourly_price : String.t option
  ; remaining_total_value : String.t option
  ; remaining_upfront_value : String.t option
  }

let make ?hourly_price ?remaining_total_value ?remaining_upfront_value () =
  { hourly_price; remaining_total_value; remaining_upfront_value }

let parse xml =
  Some
    { hourly_price = Aws.Util.option_bind (Aws.Xml.member "hourlyPrice" xml) String.parse
    ; remaining_total_value =
        Aws.Util.option_bind (Aws.Xml.member "remainingTotalValue" xml) String.parse
    ; remaining_upfront_value =
        Aws.Util.option_bind (Aws.Xml.member "remainingUpfrontValue" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remaining_upfront_value (fun f ->
             Aws.Query.Pair ("RemainingUpfrontValue", String.to_query f))
       ; Aws.Util.option_map v.remaining_total_value (fun f ->
             Aws.Query.Pair ("RemainingTotalValue", String.to_query f))
       ; Aws.Util.option_map v.hourly_price (fun f ->
             Aws.Query.Pair ("HourlyPrice", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remaining_upfront_value (fun f ->
             "remainingUpfrontValue", String.to_json f)
       ; Aws.Util.option_map v.remaining_total_value (fun f ->
             "remainingTotalValue", String.to_json f)
       ; Aws.Util.option_map v.hourly_price (fun f -> "hourlyPrice", String.to_json f)
       ])

let of_json j =
  { hourly_price = Aws.Util.option_map (Aws.Json.lookup j "hourlyPrice") String.of_json
  ; remaining_total_value =
      Aws.Util.option_map (Aws.Json.lookup j "remainingTotalValue") String.of_json
  ; remaining_upfront_value =
      Aws.Util.option_map (Aws.Json.lookup j "remainingUpfrontValue") String.of_json
  }
