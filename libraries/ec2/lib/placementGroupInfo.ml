type t = { supported_strategies : PlacementGroupStrategyList.t }

let make ?(supported_strategies = []) () = { supported_strategies }

let parse xml =
  Some
    { supported_strategies =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedStrategies" xml)
             PlacementGroupStrategyList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedStrategies"
              , PlacementGroupStrategyList.to_query v.supported_strategies ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "supportedStrategies"
           , PlacementGroupStrategyList.to_json v.supported_strategies )
       ])

let of_json j =
  { supported_strategies =
      PlacementGroupStrategyList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedStrategies"))
  }
