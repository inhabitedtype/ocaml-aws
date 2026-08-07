open Aws.BaseTypes

type t =
  { pool_id : String.t option
  ; description : String.t option
  ; pool_address_ranges : PublicIpv4PoolRangeSet.t
  ; total_address_count : Integer.t option
  ; total_available_address_count : Integer.t option
  ; network_border_group : String.t option
  ; tags : TagList.t
  }

let make
    ?pool_id
    ?description
    ?(pool_address_ranges = [])
    ?total_address_count
    ?total_available_address_count
    ?network_border_group
    ?(tags = [])
    () =
  { pool_id
  ; description
  ; pool_address_ranges
  ; total_address_count
  ; total_available_address_count
  ; network_border_group
  ; tags
  }

let parse xml =
  Some
    { pool_id = Aws.Util.option_bind (Aws.Xml.member "poolId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; pool_address_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "poolAddressRangeSet" xml)
             PublicIpv4PoolRangeSet.parse)
    ; total_address_count =
        Aws.Util.option_bind (Aws.Xml.member "totalAddressCount" xml) Integer.parse
    ; total_available_address_count =
        Aws.Util.option_bind
          (Aws.Xml.member "totalAvailableAddressCount" xml)
          Integer.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "networkBorderGroup" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.total_available_address_count (fun f ->
             Aws.Query.Pair ("TotalAvailableAddressCount", Integer.to_query f))
       ; Aws.Util.option_map v.total_address_count (fun f ->
             Aws.Query.Pair ("TotalAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PoolAddressRangeSet"
              , PublicIpv4PoolRangeSet.to_query v.pool_address_ranges ))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.pool_id (fun f ->
             Aws.Query.Pair ("PoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "networkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.total_available_address_count (fun f ->
             "totalAvailableAddressCount", Integer.to_json f)
       ; Aws.Util.option_map v.total_address_count (fun f ->
             "totalAddressCount", Integer.to_json f)
       ; Some ("poolAddressRangeSet", PublicIpv4PoolRangeSet.to_json v.pool_address_ranges)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.pool_id (fun f -> "poolId", String.to_json f)
       ])

let of_json j =
  { pool_id = Aws.Util.option_map (Aws.Json.lookup j "poolId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; pool_address_ranges =
      PublicIpv4PoolRangeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "poolAddressRangeSet"))
  ; total_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "totalAddressCount") Integer.of_json
  ; total_available_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "totalAvailableAddressCount") Integer.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "networkBorderGroup") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
