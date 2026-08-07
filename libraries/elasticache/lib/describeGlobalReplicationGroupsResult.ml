open Aws.BaseTypes

type t =
  { marker : String.t option
  ; global_replication_groups : GlobalReplicationGroupList.t
  }

let make ?marker ?(global_replication_groups = []) () =
  { marker; global_replication_groups }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; global_replication_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalReplicationGroups" xml)
             GlobalReplicationGroupList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "GlobalReplicationGroups.member"
              , GlobalReplicationGroupList.to_query v.global_replication_groups ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "GlobalReplicationGroups"
           , GlobalReplicationGroupList.to_json v.global_replication_groups )
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; global_replication_groups =
      GlobalReplicationGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroups"))
  }
