type t =
  { image_providers : ImageProviderList.t
  ; marketplace_product_codes : MarketplaceProductCodeList.t
  ; image_names : ImageNameList.t
  ; deprecation_time_condition : DeprecationTimeCondition.t option
  ; creation_date_condition : CreationDateCondition.t option
  ; image_watermarks : ImageWatermarkFilterResponseList.t
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
             (Aws.Xml.member "imageProviderSet" xml)
             ImageProviderList.parse)
    ; marketplace_product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "marketplaceProductCodeSet" xml)
             MarketplaceProductCodeList.parse)
    ; image_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "imageNameSet" xml) ImageNameList.parse)
    ; deprecation_time_condition =
        Aws.Util.option_bind
          (Aws.Xml.member "deprecationTimeCondition" xml)
          DeprecationTimeCondition.parse
    ; creation_date_condition =
        Aws.Util.option_bind
          (Aws.Xml.member "creationDateCondition" xml)
          CreationDateCondition.parse
    ; image_watermarks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "imageWatermarkSet" xml)
             ImageWatermarkFilterResponseList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ImageWatermarkSet"
              , ImageWatermarkFilterResponseList.to_query v.image_watermarks ))
       ; Aws.Util.option_map v.creation_date_condition (fun f ->
             Aws.Query.Pair ("CreationDateCondition", CreationDateCondition.to_query f))
       ; Aws.Util.option_map v.deprecation_time_condition (fun f ->
             Aws.Query.Pair
               ("DeprecationTimeCondition", DeprecationTimeCondition.to_query f))
       ; Some (Aws.Query.Pair ("ImageNameSet", ImageNameList.to_query v.image_names))
       ; Some
           (Aws.Query.Pair
              ( "MarketplaceProductCodeSet"
              , MarketplaceProductCodeList.to_query v.marketplace_product_codes ))
       ; Some
           (Aws.Query.Pair
              ("ImageProviderSet", ImageProviderList.to_query v.image_providers))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "imageWatermarkSet"
           , ImageWatermarkFilterResponseList.to_json v.image_watermarks )
       ; Aws.Util.option_map v.creation_date_condition (fun f ->
             "creationDateCondition", CreationDateCondition.to_json f)
       ; Aws.Util.option_map v.deprecation_time_condition (fun f ->
             "deprecationTimeCondition", DeprecationTimeCondition.to_json f)
       ; Some ("imageNameSet", ImageNameList.to_json v.image_names)
       ; Some
           ( "marketplaceProductCodeSet"
           , MarketplaceProductCodeList.to_json v.marketplace_product_codes )
       ; Some ("imageProviderSet", ImageProviderList.to_json v.image_providers)
       ])

let of_json j =
  { image_providers =
      ImageProviderList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageProviderSet"))
  ; marketplace_product_codes =
      MarketplaceProductCodeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "marketplaceProductCodeSet"))
  ; image_names =
      ImageNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imageNameSet"))
  ; deprecation_time_condition =
      Aws.Util.option_map
        (Aws.Json.lookup j "deprecationTimeCondition")
        DeprecationTimeCondition.of_json
  ; creation_date_condition =
      Aws.Util.option_map
        (Aws.Json.lookup j "creationDateCondition")
        CreationDateCondition.of_json
  ; image_watermarks =
      ImageWatermarkFilterResponseList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageWatermarkSet"))
  }
