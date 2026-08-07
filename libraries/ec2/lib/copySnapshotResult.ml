open Aws.BaseTypes

type t =
  { tags : TagList.t
  ; snapshot_id : String.t option
  }

let make ?(tags = []) ?snapshot_id () = { tags; snapshot_id }

let parse xml =
  Some
    { tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ])

let of_json j =
  { tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  }
