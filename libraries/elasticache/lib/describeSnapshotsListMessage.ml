open Aws.BaseTypes

type t =
  { marker : String.t option
  ; snapshots : SnapshotList.t
  }

let make ?marker ?(snapshots = []) () = { marker; snapshots }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; snapshots =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Snapshots" xml) SnapshotList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Snapshots.member", SnapshotList.to_query v.snapshots))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Snapshots", SnapshotList.to_json v.snapshots)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; snapshots =
      SnapshotList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Snapshots"))
  }
