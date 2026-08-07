open Aws.BaseTypes

type t =
  { serverless_v2_platform_version : String.t option
  ; serverless_v2_platform_version_description : String.t option
  ; engine : String.t option
  ; serverless_v2_features_support : ServerlessV2FeaturesSupport.t option
  ; status : String.t option
  ; is_default : Boolean.t option
  }

let make
    ?serverless_v2_platform_version
    ?serverless_v2_platform_version_description
    ?engine
    ?serverless_v2_features_support
    ?status
    ?is_default
    () =
  { serverless_v2_platform_version
  ; serverless_v2_platform_version_description
  ; engine
  ; serverless_v2_features_support
  ; status
  ; is_default
  }

let parse xml =
  Some
    { serverless_v2_platform_version =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2PlatformVersion" xml)
          String.parse
    ; serverless_v2_platform_version_description =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2PlatformVersionDescription" xml)
          String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; serverless_v2_features_support =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2FeaturesSupport" xml)
          ServerlessV2FeaturesSupport.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "IsDefault" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("IsDefault", Boolean.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.serverless_v2_features_support (fun f ->
             Aws.Query.Pair
               ("ServerlessV2FeaturesSupport", ServerlessV2FeaturesSupport.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.serverless_v2_platform_version_description (fun f ->
             Aws.Query.Pair ("ServerlessV2PlatformVersionDescription", String.to_query f))
       ; Aws.Util.option_map v.serverless_v2_platform_version (fun f ->
             Aws.Query.Pair ("ServerlessV2PlatformVersion", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_default (fun f -> "IsDefault", Boolean.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.serverless_v2_features_support (fun f ->
             "ServerlessV2FeaturesSupport", ServerlessV2FeaturesSupport.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.serverless_v2_platform_version_description (fun f ->
             "ServerlessV2PlatformVersionDescription", String.to_json f)
       ; Aws.Util.option_map v.serverless_v2_platform_version (fun f ->
             "ServerlessV2PlatformVersion", String.to_json f)
       ])

let of_json j =
  { serverless_v2_platform_version =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessV2PlatformVersion") String.of_json
  ; serverless_v2_platform_version_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessV2PlatformVersionDescription")
        String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; serverless_v2_features_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessV2FeaturesSupport")
        ServerlessV2FeaturesSupport.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "IsDefault") Boolean.of_json
  }
