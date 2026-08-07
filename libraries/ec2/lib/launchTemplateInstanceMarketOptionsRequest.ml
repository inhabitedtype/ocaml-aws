type t =
  { market_type : MarketType.t option
  ; spot_options : LaunchTemplateSpotMarketOptionsRequest.t option
  }

let make ?market_type ?spot_options () = { market_type; spot_options }

let parse xml =
  Some
    { market_type =
        Aws.Util.option_bind (Aws.Xml.member "MarketType" xml) MarketType.parse
    ; spot_options =
        Aws.Util.option_bind
          (Aws.Xml.member "SpotOptions" xml)
          LaunchTemplateSpotMarketOptionsRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_options (fun f ->
             Aws.Query.Pair
               ("SpotOptions", LaunchTemplateSpotMarketOptionsRequest.to_query f))
       ; Aws.Util.option_map v.market_type (fun f ->
             Aws.Query.Pair ("MarketType", MarketType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_options (fun f ->
             "SpotOptions", LaunchTemplateSpotMarketOptionsRequest.to_json f)
       ; Aws.Util.option_map v.market_type (fun f -> "MarketType", MarketType.to_json f)
       ])

let of_json j =
  { market_type = Aws.Util.option_map (Aws.Json.lookup j "MarketType") MarketType.of_json
  ; spot_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "SpotOptions")
        LaunchTemplateSpotMarketOptionsRequest.of_json
  }
