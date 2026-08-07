open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t
  ; primary_region : String.t
  ; primary_replication_group_id : String.t
  }

let make ~global_replication_group_id ~primary_region ~primary_replication_group_id () =
  { global_replication_group_id; primary_region; primary_replication_group_id }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Xml.required
          "GlobalReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroupId" xml)
             String.parse)
    ; primary_region =
        Aws.Xml.required
          "PrimaryRegion"
          (Aws.Util.option_bind (Aws.Xml.member "PrimaryRegion" xml) String.parse)
    ; primary_replication_group_id =
        Aws.Xml.required
          "PrimaryReplicationGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "PrimaryReplicationGroupId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("PrimaryReplicationGroupId", String.to_query v.primary_replication_group_id))
       ; Some (Aws.Query.Pair ("PrimaryRegion", String.to_query v.primary_region))
       ; Some
           (Aws.Query.Pair
              ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PrimaryReplicationGroupId", String.to_json v.primary_replication_group_id)
       ; Some ("PrimaryRegion", String.to_json v.primary_region)
       ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
       ])

let of_json j =
  { global_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
  ; primary_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryRegion"))
  ; primary_replication_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryReplicationGroupId"))
  }
