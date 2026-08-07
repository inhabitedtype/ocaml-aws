open Aws.BaseTypes

type t =
  { marker : String.t option
  ; cache_engine_versions : CacheEngineVersionList.t
  }

let make ?marker ?(cache_engine_versions = []) () = { marker; cache_engine_versions }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; cache_engine_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheEngineVersions" xml)
             CacheEngineVersionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CacheEngineVersions.member"
              , CacheEngineVersionList.to_query v.cache_engine_versions ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("CacheEngineVersions", CacheEngineVersionList.to_json v.cache_engine_versions)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; cache_engine_versions =
      CacheEngineVersionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheEngineVersions"))
  }
