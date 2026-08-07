open Aws.BaseTypes

type t = { snapshot_id : String.t option }

let make ?snapshot_id () = { snapshot_id }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f) ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json }
