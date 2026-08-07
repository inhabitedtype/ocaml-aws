open Aws.BaseTypes

type t =
  { marker : String.t option
  ; reserved_cache_nodes : ReservedCacheNodeList.t
  }

let make ?marker ?(reserved_cache_nodes = []) () = { marker; reserved_cache_nodes }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; reserved_cache_nodes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedCacheNodes" xml)
             ReservedCacheNodeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedCacheNodes.member"
              , ReservedCacheNodeList.to_query v.reserved_cache_nodes ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReservedCacheNodes", ReservedCacheNodeList.to_json v.reserved_cache_nodes)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; reserved_cache_nodes =
      ReservedCacheNodeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedCacheNodes"))
  }
