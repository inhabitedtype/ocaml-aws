type t =
  { image_providers : ImageProviderRequestList.t
  ; marketplace_product_codes : MarketplaceProductCodeRequestList.t
  ; image_names : ImageNameCriteriaRequestList.t
  ; deprecation_time_condition : DeprecationTimeConditionRequest.t option
  ; creation_date_condition : CreationDateConditionRequest.t option
  ; image_watermarks : ImageWatermarkFilterRequestList.t
  }

let make
    ?(image_providers = [])
    ?(marketplace_product_codes = [])
    ?(image_names = [])
    ?deprecation_time_condition
    ?creation_date_condition
    ?(image_watermarks = [])
    () =
  { image_providers
  ; marketplace_product_codes
  ; image_names
  ; deprecation_time_condition
  ; creation_date_condition
  ; image_watermarks
  }

let parse xml =
  Some
    { image_providers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ImageProvider" xml)
             ImageProviderRequestList.parse)
    ; marketplace_product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MarketplaceProductCode" xml)
             MarketplaceProductCodeRequestList.parse)
    ; image_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ImageName" xml)
             ImageNameCriteriaRequestList.parse)
    ; deprecation_time_condition =
        Aws.Util.option_bind
          (Aws.Xml.member "DeprecationTimeCondition" xml)
          DeprecationTimeConditionRequest.parse
    ; creation_date_condition =
        Aws.Util.option_bind
          (Aws.Xml.member "CreationDateCondition" xml)
          CreationDateConditionRequest.parse
    ; image_watermarks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ImageWatermark" xml)
             ImageWatermarkFilterRequestList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ImageWatermark"
              , ImageWatermarkFilterRequestList.to_query v.image_watermarks ))
       ; Aws.Util.option_map v.creation_date_condition (fun f ->
             Aws.Query.Pair
               ("CreationDateCondition", CreationDateConditionRequest.to_query f))
       ; Aws.Util.option_map v.deprecation_time_condition (fun f ->
             Aws.Query.Pair
               ("DeprecationTimeCondition", DeprecationTimeConditionRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ImageName", ImageNameCriteriaRequestList.to_query v.image_names))
       ; Some
           (Aws.Query.Pair
              ( "MarketplaceProductCode"
              , MarketplaceProductCodeRequestList.to_query v.marketplace_product_codes ))
       ; Some
           (Aws.Query.Pair
              ("ImageProvider", ImageProviderRequestList.to_query v.image_providers))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("ImageWatermark", ImageWatermarkFilterRequestList.to_json v.image_watermarks)
       ; Aws.Util.option_map v.creation_date_condition (fun f ->
             "CreationDateCondition", CreationDateConditionRequest.to_json f)
       ; Aws.Util.option_map v.deprecation_time_condition (fun f ->
             "DeprecationTimeCondition", DeprecationTimeConditionRequest.to_json f)
       ; Some ("ImageName", ImageNameCriteriaRequestList.to_json v.image_names)
       ; Some
           ( "MarketplaceProductCode"
           , MarketplaceProductCodeRequestList.to_json v.marketplace_product_codes )
       ; Some ("ImageProvider", ImageProviderRequestList.to_json v.image_providers)
       ])

let of_json j =
  { image_providers =
      ImageProviderRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageProvider"))
  ; marketplace_product_codes =
      MarketplaceProductCodeRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MarketplaceProductCode"))
  ; image_names =
      ImageNameCriteriaRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageName"))
  ; deprecation_time_condition =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeprecationTimeCondition")
        DeprecationTimeConditionRequest.of_json
  ; creation_date_condition =
      Aws.Util.option_map
        (Aws.Json.lookup j "CreationDateCondition")
        CreationDateConditionRequest.of_json
  ; image_watermarks =
      ImageWatermarkFilterRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageWatermark"))
  }
