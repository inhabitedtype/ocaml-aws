open Aws.BaseTypes

type t =
  { marker : String.t option
  ; serverless_v2_platform_versions : ServerlessV2PlatformVersionList.t
  }

let make ?marker ?(serverless_v2_platform_versions = []) () =
  { marker; serverless_v2_platform_versions }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; serverless_v2_platform_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ServerlessV2PlatformVersions" xml)
             ServerlessV2PlatformVersionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ServerlessV2PlatformVersions.member"
              , ServerlessV2PlatformVersionList.to_query v.serverless_v2_platform_versions
              ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ServerlessV2PlatformVersions"
           , ServerlessV2PlatformVersionList.to_json v.serverless_v2_platform_versions )
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; serverless_v2_platform_versions =
      ServerlessV2PlatformVersionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessV2PlatformVersions"))
  }
