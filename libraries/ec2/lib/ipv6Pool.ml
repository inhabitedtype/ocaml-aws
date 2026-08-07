open Aws.BaseTypes

type t =
  { pool_id : String.t option
  ; description : String.t option
  ; pool_cidr_blocks : PoolCidrBlocksSet.t
  ; tags : TagList.t
  }

let make ?pool_id ?description ?(pool_cidr_blocks = []) ?(tags = []) () =
  { pool_id; description; pool_cidr_blocks; tags }

let parse xml =
  Some
    { pool_id = Aws.Util.option_bind (Aws.Xml.member "poolId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; pool_cidr_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "poolCidrBlockSet" xml)
             PoolCidrBlocksSet.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("PoolCidrBlockSet", PoolCidrBlocksSet.to_query v.pool_cidr_blocks))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.pool_id (fun f ->
             Aws.Query.Pair ("PoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Some ("poolCidrBlockSet", PoolCidrBlocksSet.to_json v.pool_cidr_blocks)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.pool_id (fun f -> "poolId", String.to_json f)
       ])

let of_json j =
  { pool_id = Aws.Util.option_map (Aws.Json.lookup j "poolId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; pool_cidr_blocks =
      PoolCidrBlocksSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "poolCidrBlockSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
