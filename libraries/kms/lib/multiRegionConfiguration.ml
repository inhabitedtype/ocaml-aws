type t =
  { multi_region_key_type : MultiRegionKeyType.t option
  ; primary_key : MultiRegionKey.t option
  ; replica_keys : MultiRegionKeyList.t
  }

let make ?multi_region_key_type ?primary_key ?(replica_keys = []) () =
  { multi_region_key_type; primary_key; replica_keys }

let parse xml =
  Some
    { multi_region_key_type =
        Aws.Util.option_bind
          (Aws.Xml.member "MultiRegionKeyType" xml)
          MultiRegionKeyType.parse
    ; primary_key =
        Aws.Util.option_bind (Aws.Xml.member "PrimaryKey" xml) MultiRegionKey.parse
    ; replica_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicaKeys" xml)
             MultiRegionKeyList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ReplicaKeys.member", MultiRegionKeyList.to_query v.replica_keys))
       ; Aws.Util.option_map v.primary_key (fun f ->
             Aws.Query.Pair ("PrimaryKey", MultiRegionKey.to_query f))
       ; Aws.Util.option_map v.multi_region_key_type (fun f ->
             Aws.Query.Pair ("MultiRegionKeyType", MultiRegionKeyType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReplicaKeys", MultiRegionKeyList.to_json v.replica_keys)
       ; Aws.Util.option_map v.primary_key (fun f ->
             "PrimaryKey", MultiRegionKey.to_json f)
       ; Aws.Util.option_map v.multi_region_key_type (fun f ->
             "MultiRegionKeyType", MultiRegionKeyType.to_json f)
       ])

let of_json j =
  { multi_region_key_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "MultiRegionKeyType")
        MultiRegionKeyType.of_json
  ; primary_key =
      Aws.Util.option_map (Aws.Json.lookup j "PrimaryKey") MultiRegionKey.of_json
  ; replica_keys =
      MultiRegionKeyList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaKeys"))
  }
