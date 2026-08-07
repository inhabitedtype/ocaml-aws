open Aws.BaseTypes

type t =
  { reserved_cache_nodes_offering_id : String.t option
  ; cache_node_type : String.t option
  ; duration : String.t option
  ; product_description : String.t option
  ; offering_type : String.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ?reserved_cache_nodes_offering_id
    ?cache_node_type
    ?duration
    ?product_description
    ?offering_type
    ?max_records
    ?marker
    () =
  { reserved_cache_nodes_offering_id
  ; cache_node_type
  ; duration
  ; product_description
  ; offering_type
  ; max_records
  ; marker
  }

let parse xml =
  Some
    { reserved_cache_nodes_offering_id =
        Aws.Util.option_bind
          (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
          String.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) String.parse
    ; product_description =
        Aws.Util.option_bind (Aws.Xml.member "ProductDescription" xml) String.parse
    ; offering_type =
        Aws.Util.option_bind (Aws.Xml.member "OfferingType" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.offering_type (fun f ->
             Aws.Query.Pair ("OfferingType", String.to_query f))
       ; Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", String.to_query f))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
             Aws.Query.Pair ("ReservedCacheNodesOfferingId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.offering_type (fun f -> "OfferingType", String.to_json f)
       ; Aws.Util.option_map v.product_description (fun f ->
             "ProductDescription", String.to_json f)
       ; Aws.Util.option_map v.duration (fun f -> "Duration", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
             "ReservedCacheNodesOfferingId", String.to_json f)
       ])

let of_json j =
  { reserved_cache_nodes_offering_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReservedCacheNodesOfferingId")
        String.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") String.of_json
  ; product_description =
      Aws.Util.option_map (Aws.Json.lookup j "ProductDescription") String.of_json
  ; offering_type = Aws.Util.option_map (Aws.Json.lookup j "OfferingType") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
