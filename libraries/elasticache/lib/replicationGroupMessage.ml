open Aws.BaseTypes

type t =
  { marker : String.t option
  ; replication_groups : ReplicationGroupList.t
  }

let make ?marker ?(replication_groups = []) () = { marker; replication_groups }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; replication_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicationGroups" xml)
             ReplicationGroupList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReplicationGroups.member"
              , ReplicationGroupList.to_query v.replication_groups ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReplicationGroups", ReplicationGroupList.to_json v.replication_groups)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; replication_groups =
      ReplicationGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroups"))
  }
