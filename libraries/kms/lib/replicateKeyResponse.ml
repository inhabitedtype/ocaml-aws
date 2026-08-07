open Aws.BaseTypes

type t =
  { replica_key_metadata : KeyMetadata.t option
  ; replica_policy : String.t option
  ; replica_tags : TagList.t
  }

let make ?replica_key_metadata ?replica_policy ?(replica_tags = []) () =
  { replica_key_metadata; replica_policy; replica_tags }

let parse xml =
  Some
    { replica_key_metadata =
        Aws.Util.option_bind (Aws.Xml.member "ReplicaKeyMetadata" xml) KeyMetadata.parse
    ; replica_policy =
        Aws.Util.option_bind (Aws.Xml.member "ReplicaPolicy" xml) String.parse
    ; replica_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ReplicaTags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ReplicaTags.member", TagList.to_query v.replica_tags))
       ; Aws.Util.option_map v.replica_policy (fun f ->
             Aws.Query.Pair ("ReplicaPolicy", String.to_query f))
       ; Aws.Util.option_map v.replica_key_metadata (fun f ->
             Aws.Query.Pair ("ReplicaKeyMetadata", KeyMetadata.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReplicaTags", TagList.to_json v.replica_tags)
       ; Aws.Util.option_map v.replica_policy (fun f -> "ReplicaPolicy", String.to_json f)
       ; Aws.Util.option_map v.replica_key_metadata (fun f ->
             "ReplicaKeyMetadata", KeyMetadata.to_json f)
       ])

let of_json j =
  { replica_key_metadata =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicaKeyMetadata") KeyMetadata.of_json
  ; replica_policy =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicaPolicy") String.of_json
  ; replica_tags =
      TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaTags"))
  }
