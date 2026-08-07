open Aws.BaseTypes

type t =
  { reserved_cache_node_id : String.t option
  ; reserved_cache_nodes_offering_id : String.t option
  ; cache_node_type : String.t option
  ; start_time : DateTime.t option
  ; duration : Integer.t option
  ; fixed_price : Double.t option
  ; usage_price : Double.t option
  ; cache_node_count : Integer.t option
  ; product_description : String.t option
  ; offering_type : String.t option
  ; state : String.t option
  ; recurring_charges : RecurringChargeList.t
  ; reservation_a_r_n : String.t option
  }

let make
    ?reserved_cache_node_id
    ?reserved_cache_nodes_offering_id
    ?cache_node_type
    ?start_time
    ?duration
    ?fixed_price
    ?usage_price
    ?cache_node_count
    ?product_description
    ?offering_type
    ?state
    ?(recurring_charges = [])
    ?reservation_a_r_n
    () =
  { reserved_cache_node_id
  ; reserved_cache_nodes_offering_id
  ; cache_node_type
  ; start_time
  ; duration
  ; fixed_price
  ; usage_price
  ; cache_node_count
  ; product_description
  ; offering_type
  ; state
  ; recurring_charges
  ; reservation_a_r_n
  }

let parse xml =
  Some
    { reserved_cache_node_id =
        Aws.Util.option_bind (Aws.Xml.member "ReservedCacheNodeId" xml) String.parse
    ; reserved_cache_nodes_offering_id =
        Aws.Util.option_bind
          (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
          String.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) Integer.parse
    ; fixed_price = Aws.Util.option_bind (Aws.Xml.member "FixedPrice" xml) Double.parse
    ; usage_price = Aws.Util.option_bind (Aws.Xml.member "UsagePrice" xml) Double.parse
    ; cache_node_count =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeCount" xml) Integer.parse
    ; product_description =
        Aws.Util.option_bind (Aws.Xml.member "ProductDescription" xml) String.parse
    ; offering_type =
        Aws.Util.option_bind (Aws.Xml.member "OfferingType" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
    ; recurring_charges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RecurringCharges" xml)
             RecurringChargeList.parse)
    ; reservation_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "ReservationARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reservation_a_r_n (fun f ->
             Aws.Query.Pair ("ReservationARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RecurringCharges.member", RecurringChargeList.to_query v.recurring_charges))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.offering_type (fun f ->
             Aws.Query.Pair ("OfferingType", String.to_query f))
       ; Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", String.to_query f))
       ; Aws.Util.option_map v.cache_node_count (fun f ->
             Aws.Query.Pair ("CacheNodeCount", Integer.to_query f))
       ; Aws.Util.option_map v.usage_price (fun f ->
             Aws.Query.Pair ("UsagePrice", Double.to_query f))
       ; Aws.Util.option_map v.fixed_price (fun f ->
             Aws.Query.Pair ("FixedPrice", Double.to_query f))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", Integer.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
             Aws.Query.Pair ("ReservedCacheNodesOfferingId", String.to_query f))
       ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
             Aws.Query.Pair ("ReservedCacheNodeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reservation_a_r_n (fun f ->
             "ReservationARN", String.to_json f)
       ; Some ("RecurringCharges", RecurringChargeList.to_json v.recurring_charges)
       ; Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
       ; Aws.Util.option_map v.offering_type (fun f -> "OfferingType", String.to_json f)
       ; Aws.Util.option_map v.product_description (fun f ->
             "ProductDescription", String.to_json f)
       ; Aws.Util.option_map v.cache_node_count (fun f ->
             "CacheNodeCount", Integer.to_json f)
       ; Aws.Util.option_map v.usage_price (fun f -> "UsagePrice", Double.to_json f)
       ; Aws.Util.option_map v.fixed_price (fun f -> "FixedPrice", Double.to_json f)
       ; Aws.Util.option_map v.duration (fun f -> "Duration", Integer.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
             "ReservedCacheNodesOfferingId", String.to_json f)
       ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
             "ReservedCacheNodeId", String.to_json f)
       ])

let of_json j =
  { reserved_cache_node_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReservedCacheNodeId") String.of_json
  ; reserved_cache_nodes_offering_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReservedCacheNodesOfferingId")
        String.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") Integer.of_json
  ; fixed_price = Aws.Util.option_map (Aws.Json.lookup j "FixedPrice") Double.of_json
  ; usage_price = Aws.Util.option_map (Aws.Json.lookup j "UsagePrice") Double.of_json
  ; cache_node_count =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCount") Integer.of_json
  ; product_description =
      Aws.Util.option_map (Aws.Json.lookup j "ProductDescription") String.of_json
  ; offering_type = Aws.Util.option_map (Aws.Json.lookup j "OfferingType") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
  ; recurring_charges =
      RecurringChargeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RecurringCharges"))
  ; reservation_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "ReservationARN") String.of_json
  }
