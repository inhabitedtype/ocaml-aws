type t = { spot_datafeed_subscription : SpotDatafeedSubscription.t option }

let make ?spot_datafeed_subscription () = { spot_datafeed_subscription }

let parse xml =
  Some
    { spot_datafeed_subscription =
        Aws.Util.option_bind
          (Aws.Xml.member "spotDatafeedSubscription" xml)
          SpotDatafeedSubscription.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_datafeed_subscription (fun f ->
             Aws.Query.Pair
               ("SpotDatafeedSubscription", SpotDatafeedSubscription.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_datafeed_subscription (fun f ->
             "spotDatafeedSubscription", SpotDatafeedSubscription.to_json f)
       ])

let of_json j =
  { spot_datafeed_subscription =
      Aws.Util.option_map
        (Aws.Json.lookup j "spotDatafeedSubscription")
        SpotDatafeedSubscription.of_json
  }
