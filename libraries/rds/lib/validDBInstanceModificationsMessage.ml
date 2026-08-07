open Aws.BaseTypes

type t =
  { storage : ValidStorageOptionsList.t
  ; valid_processor_features : AvailableProcessorFeatureList.t
  ; supports_dedicated_log_volume : Boolean.t option
  ; additional_storage : ValidAdditionalStorageOptions.t option
  }

let make
    ?(storage = [])
    ?(valid_processor_features = [])
    ?supports_dedicated_log_volume
    ?additional_storage
    () =
  { storage; valid_processor_features; supports_dedicated_log_volume; additional_storage }

let parse xml =
  Some
    { storage =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Storage" xml)
             ValidStorageOptionsList.parse)
    ; valid_processor_features =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ValidProcessorFeatures" xml)
             AvailableProcessorFeatureList.parse)
    ; supports_dedicated_log_volume =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsDedicatedLogVolume" xml)
          Boolean.parse
    ; additional_storage =
        Aws.Util.option_bind
          (Aws.Xml.member "AdditionalStorage" xml)
          ValidAdditionalStorageOptions.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.additional_storage (fun f ->
             Aws.Query.Pair ("AdditionalStorage", ValidAdditionalStorageOptions.to_query f))
       ; Aws.Util.option_map v.supports_dedicated_log_volume (fun f ->
             Aws.Query.Pair ("SupportsDedicatedLogVolume", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ValidProcessorFeatures.member"
              , AvailableProcessorFeatureList.to_query v.valid_processor_features ))
       ; Some
           (Aws.Query.Pair ("Storage.member", ValidStorageOptionsList.to_query v.storage))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.additional_storage (fun f ->
             "AdditionalStorage", ValidAdditionalStorageOptions.to_json f)
       ; Aws.Util.option_map v.supports_dedicated_log_volume (fun f ->
             "SupportsDedicatedLogVolume", Boolean.to_json f)
       ; Some
           ( "ValidProcessorFeatures"
           , AvailableProcessorFeatureList.to_json v.valid_processor_features )
       ; Some ("Storage", ValidStorageOptionsList.to_json v.storage)
       ])

let of_json j =
  { storage =
      ValidStorageOptionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Storage"))
  ; valid_processor_features =
      AvailableProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ValidProcessorFeatures"))
  ; supports_dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsDedicatedLogVolume") Boolean.of_json
  ; additional_storage =
      Aws.Util.option_map
        (Aws.Json.lookup j "AdditionalStorage")
        ValidAdditionalStorageOptions.of_json
  }
