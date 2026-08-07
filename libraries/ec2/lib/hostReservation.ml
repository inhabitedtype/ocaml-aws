open Aws.BaseTypes

type t =
  { count : Integer.t option
  ; currency_code : CurrencyCodeValues.t option
  ; duration : Integer.t option
  ; end_ : DateTime.t option
  ; host_id_set : ResponseHostIdSet.t
  ; host_reservation_id : String.t option
  ; hourly_price : String.t option
  ; instance_family : String.t option
  ; offering_id : String.t option
  ; payment_option : PaymentOption.t option
  ; start : DateTime.t option
  ; state : ReservationState.t option
  ; upfront_price : String.t option
  ; tags : TagList.t
  }

let make
    ?count
    ?currency_code
    ?duration
    ?end_
    ?(host_id_set = [])
    ?host_reservation_id
    ?hourly_price
    ?instance_family
    ?offering_id
    ?payment_option
    ?start
    ?state
    ?upfront_price
    ?(tags = [])
    () =
  { count
  ; currency_code
  ; duration
  ; end_
  ; host_id_set
  ; host_reservation_id
  ; hourly_price
  ; instance_family
  ; offering_id
  ; payment_option
  ; start
  ; state
  ; upfront_price
  ; tags
  }

let parse xml =
  Some
    { count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "duration" xml) Integer.parse
    ; end_ = Aws.Util.option_bind (Aws.Xml.member "end" xml) DateTime.parse
    ; host_id_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "hostIdSet" xml) ResponseHostIdSet.parse)
    ; host_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "hostReservationId" xml) String.parse
    ; hourly_price = Aws.Util.option_bind (Aws.Xml.member "hourlyPrice" xml) String.parse
    ; instance_family =
        Aws.Util.option_bind (Aws.Xml.member "instanceFamily" xml) String.parse
    ; offering_id = Aws.Util.option_bind (Aws.Xml.member "offeringId" xml) String.parse
    ; payment_option =
        Aws.Util.option_bind (Aws.Xml.member "paymentOption" xml) PaymentOption.parse
    ; start = Aws.Util.option_bind (Aws.Xml.member "start" xml) DateTime.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) ReservationState.parse
    ; upfront_price =
        Aws.Util.option_bind (Aws.Xml.member "upfrontPrice" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.upfront_price (fun f ->
             Aws.Query.Pair ("UpfrontPrice", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", ReservationState.to_query f))
       ; Aws.Util.option_map v.start (fun f ->
             Aws.Query.Pair ("Start", DateTime.to_query f))
       ; Aws.Util.option_map v.payment_option (fun f ->
             Aws.Query.Pair ("PaymentOption", PaymentOption.to_query f))
       ; Aws.Util.option_map v.offering_id (fun f ->
             Aws.Query.Pair ("OfferingId", String.to_query f))
       ; Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", String.to_query f))
       ; Aws.Util.option_map v.hourly_price (fun f ->
             Aws.Query.Pair ("HourlyPrice", String.to_query f))
       ; Aws.Util.option_map v.host_reservation_id (fun f ->
             Aws.Query.Pair ("HostReservationId", String.to_query f))
       ; Some (Aws.Query.Pair ("HostIdSet", ResponseHostIdSet.to_query v.host_id_set))
       ; Aws.Util.option_map v.end_ (fun f -> Aws.Query.Pair ("End", DateTime.to_query f))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", Integer.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.upfront_price (fun f -> "upfrontPrice", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", ReservationState.to_json f)
       ; Aws.Util.option_map v.start (fun f -> "start", DateTime.to_json f)
       ; Aws.Util.option_map v.payment_option (fun f ->
             "paymentOption", PaymentOption.to_json f)
       ; Aws.Util.option_map v.offering_id (fun f -> "offeringId", String.to_json f)
       ; Aws.Util.option_map v.instance_family (fun f ->
             "instanceFamily", String.to_json f)
       ; Aws.Util.option_map v.hourly_price (fun f -> "hourlyPrice", String.to_json f)
       ; Aws.Util.option_map v.host_reservation_id (fun f ->
             "hostReservationId", String.to_json f)
       ; Some ("hostIdSet", ResponseHostIdSet.to_json v.host_id_set)
       ; Aws.Util.option_map v.end_ (fun f -> "end", DateTime.to_json f)
       ; Aws.Util.option_map v.duration (fun f -> "duration", Integer.to_json f)
       ; Aws.Util.option_map v.currency_code (fun f ->
             "currencyCode", CurrencyCodeValues.to_json f)
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ])

let of_json j =
  { count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "currencyCode") CurrencyCodeValues.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "duration") Integer.of_json
  ; end_ = Aws.Util.option_map (Aws.Json.lookup j "end") DateTime.of_json
  ; host_id_set =
      ResponseHostIdSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostIdSet"))
  ; host_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "hostReservationId") String.of_json
  ; hourly_price = Aws.Util.option_map (Aws.Json.lookup j "hourlyPrice") String.of_json
  ; instance_family =
      Aws.Util.option_map (Aws.Json.lookup j "instanceFamily") String.of_json
  ; offering_id = Aws.Util.option_map (Aws.Json.lookup j "offeringId") String.of_json
  ; payment_option =
      Aws.Util.option_map (Aws.Json.lookup j "paymentOption") PaymentOption.of_json
  ; start = Aws.Util.option_map (Aws.Json.lookup j "start") DateTime.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") ReservationState.of_json
  ; upfront_price = Aws.Util.option_map (Aws.Json.lookup j "upfrontPrice") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
