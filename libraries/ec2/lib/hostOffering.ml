open Aws.BaseTypes

type t =
  { currency_code : CurrencyCodeValues.t option
  ; duration : Integer.t option
  ; hourly_price : String.t option
  ; instance_family : String.t option
  ; offering_id : String.t option
  ; payment_option : PaymentOption.t option
  ; upfront_price : String.t option
  }

let make
    ?currency_code
    ?duration
    ?hourly_price
    ?instance_family
    ?offering_id
    ?payment_option
    ?upfront_price
    () =
  { currency_code
  ; duration
  ; hourly_price
  ; instance_family
  ; offering_id
  ; payment_option
  ; upfront_price
  }

let parse xml =
  Some
    { currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "duration" xml) Integer.parse
    ; hourly_price = Aws.Util.option_bind (Aws.Xml.member "hourlyPrice" xml) String.parse
    ; instance_family =
        Aws.Util.option_bind (Aws.Xml.member "instanceFamily" xml) String.parse
    ; offering_id = Aws.Util.option_bind (Aws.Xml.member "offeringId" xml) String.parse
    ; payment_option =
        Aws.Util.option_bind (Aws.Xml.member "paymentOption" xml) PaymentOption.parse
    ; upfront_price =
        Aws.Util.option_bind (Aws.Xml.member "upfrontPrice" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.upfront_price (fun f ->
             Aws.Query.Pair ("UpfrontPrice", String.to_query f))
       ; Aws.Util.option_map v.payment_option (fun f ->
             Aws.Query.Pair ("PaymentOption", PaymentOption.to_query f))
       ; Aws.Util.option_map v.offering_id (fun f ->
             Aws.Query.Pair ("OfferingId", String.to_query f))
       ; Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", String.to_query f))
       ; Aws.Util.option_map v.hourly_price (fun f ->
             Aws.Query.Pair ("HourlyPrice", String.to_query f))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", Integer.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.upfront_price (fun f -> "upfrontPrice", String.to_json f)
       ; Aws.Util.option_map v.payment_option (fun f ->
             "paymentOption", PaymentOption.to_json f)
       ; Aws.Util.option_map v.offering_id (fun f -> "offeringId", String.to_json f)
       ; Aws.Util.option_map v.instance_family (fun f ->
             "instanceFamily", String.to_json f)
       ; Aws.Util.option_map v.hourly_price (fun f -> "hourlyPrice", String.to_json f)
       ; Aws.Util.option_map v.duration (fun f -> "duration", Integer.to_json f)
       ; Aws.Util.option_map v.currency_code (fun f ->
             "currencyCode", CurrencyCodeValues.to_json f)
       ])

let of_json j =
  { currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "currencyCode") CurrencyCodeValues.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "duration") Integer.of_json
  ; hourly_price = Aws.Util.option_map (Aws.Json.lookup j "hourlyPrice") String.of_json
  ; instance_family =
      Aws.Util.option_map (Aws.Json.lookup j "instanceFamily") String.of_json
  ; offering_id = Aws.Util.option_map (Aws.Json.lookup j "offeringId") String.of_json
  ; payment_option =
      Aws.Util.option_map (Aws.Json.lookup j "paymentOption") PaymentOption.of_json
  ; upfront_price = Aws.Util.option_map (Aws.Json.lookup j "upfrontPrice") String.of_json
  }
