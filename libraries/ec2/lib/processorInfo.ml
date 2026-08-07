open Aws.BaseTypes

type t =
  { supported_architectures : ArchitectureTypeList.t
  ; sustained_clock_speed_in_ghz : Double.t option
  ; supported_features : SupportedAdditionalProcessorFeatureList.t
  ; manufacturer : String.t option
  }

let make
    ?(supported_architectures = [])
    ?sustained_clock_speed_in_ghz
    ?(supported_features = [])
    ?manufacturer
    () =
  { supported_architectures
  ; sustained_clock_speed_in_ghz
  ; supported_features
  ; manufacturer
  }

let parse xml =
  Some
    { supported_architectures =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedArchitectures" xml)
             ArchitectureTypeList.parse)
    ; sustained_clock_speed_in_ghz =
        Aws.Util.option_bind (Aws.Xml.member "sustainedClockSpeedInGhz" xml) Double.parse
    ; supported_features =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedFeatures" xml)
             SupportedAdditionalProcessorFeatureList.parse)
    ; manufacturer = Aws.Util.option_bind (Aws.Xml.member "manufacturer" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.manufacturer (fun f ->
             Aws.Query.Pair ("Manufacturer", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedFeatures"
              , SupportedAdditionalProcessorFeatureList.to_query v.supported_features ))
       ; Aws.Util.option_map v.sustained_clock_speed_in_ghz (fun f ->
             Aws.Query.Pair ("SustainedClockSpeedInGhz", Double.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedArchitectures"
              , ArchitectureTypeList.to_query v.supported_architectures ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.manufacturer (fun f -> "manufacturer", String.to_json f)
       ; Some
           ( "supportedFeatures"
           , SupportedAdditionalProcessorFeatureList.to_json v.supported_features )
       ; Aws.Util.option_map v.sustained_clock_speed_in_ghz (fun f ->
             "sustainedClockSpeedInGhz", Double.to_json f)
       ; Some
           ( "supportedArchitectures"
           , ArchitectureTypeList.to_json v.supported_architectures )
       ])

let of_json j =
  { supported_architectures =
      ArchitectureTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedArchitectures"))
  ; sustained_clock_speed_in_ghz =
      Aws.Util.option_map (Aws.Json.lookup j "sustainedClockSpeedInGhz") Double.of_json
  ; supported_features =
      SupportedAdditionalProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedFeatures"))
  ; manufacturer = Aws.Util.option_map (Aws.Json.lookup j "manufacturer") String.of_json
  }
