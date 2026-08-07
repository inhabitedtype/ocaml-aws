open Aws.BaseTypes

type t =
  { reserved_cache_nodes_offering_id : String.t
  ; reserved_cache_node_id : String.t option
  ; cache_node_count : Integer.t option
  ; tags : TagList.t
  }

let make
    ~reserved_cache_nodes_offering_id
    ?reserved_cache_node_id
    ?cache_node_count
    ?(tags = [])
    () =
  { reserved_cache_nodes_offering_id; reserved_cache_node_id; cache_node_count; tags }

let parse xml =
  Some
    { reserved_cache_nodes_offering_id =
        Aws.Xml.required
          "ReservedCacheNodesOfferingId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
             String.parse)
    ; reserved_cache_node_id =
        Aws.Util.option_bind (Aws.Xml.member "ReservedCacheNodeId" xml) String.parse
    ; cache_node_count =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeCount" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.cache_node_count (fun f ->
             Aws.Query.Pair ("CacheNodeCount", Integer.to_query f))
       ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
             Aws.Query.Pair ("ReservedCacheNodeId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedCacheNodesOfferingId"
              , String.to_query v.reserved_cache_nodes_offering_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.cache_node_count (fun f ->
             "CacheNodeCount", Integer.to_json f)
       ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
             "ReservedCacheNodeId", String.to_json f)
       ; Some
           ( "ReservedCacheNodesOfferingId"
           , String.to_json v.reserved_cache_nodes_offering_id )
       ])

let of_json j =
  { reserved_cache_nodes_offering_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedCacheNodesOfferingId"))
  ; reserved_cache_node_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReservedCacheNodeId") String.of_json
  ; cache_node_count =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCount") Integer.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
