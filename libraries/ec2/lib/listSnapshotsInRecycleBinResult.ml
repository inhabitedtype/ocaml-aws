open Aws.BaseTypes

type t =
  { snapshots : SnapshotRecycleBinInfoList.t
  ; next_token : String.t option
  }

let make ?(snapshots = []) ?next_token () = { snapshots; next_token }

let parse xml =
  Some
    { snapshots =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "snapshotSet" xml)
             SnapshotRecycleBinInfoList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("SnapshotSet", SnapshotRecycleBinInfoList.to_query v.snapshots))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("snapshotSet", SnapshotRecycleBinInfoList.to_json v.snapshots)
       ])

let of_json j =
  { snapshots =
      SnapshotRecycleBinInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "snapshotSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
