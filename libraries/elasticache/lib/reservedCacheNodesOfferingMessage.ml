open Aws.BaseTypes

type t =
  { marker : String.t option
  ; reserved_cache_nodes_offerings : ReservedCacheNodesOfferingList.t
  }

let make ?marker ?(reserved_cache_nodes_offerings = []) () =
  { marker; reserved_cache_nodes_offerings }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; reserved_cache_nodes_offerings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedCacheNodesOfferings" xml)
             ReservedCacheNodesOfferingList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedCacheNodesOfferings.member"
              , ReservedCacheNodesOfferingList.to_query v.reserved_cache_nodes_offerings
              ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ReservedCacheNodesOfferings"
           , ReservedCacheNodesOfferingList.to_json v.reserved_cache_nodes_offerings )
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; reserved_cache_nodes_offerings =
      ReservedCacheNodesOfferingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedCacheNodesOfferings"))
  }
