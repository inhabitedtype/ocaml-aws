open Aws.BaseTypes

type t =
  { snapshot_id : String.t
  ; storage_tier : TargetStorageTier.t option
  ; dry_run : Boolean.t option
  }

let make ~snapshot_id ?storage_tier ?dry_run () = { snapshot_id; storage_tier; dry_run }

let parse xml =
  Some
    { snapshot_id =
        Aws.Xml.required
          "SnapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse)
    ; storage_tier =
        Aws.Util.option_bind (Aws.Xml.member "StorageTier" xml) TargetStorageTier.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.storage_tier (fun f ->
             Aws.Query.Pair ("StorageTier", TargetStorageTier.to_query f))
       ; Some (Aws.Query.Pair ("SnapshotId", String.to_query v.snapshot_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.storage_tier (fun f ->
             "StorageTier", TargetStorageTier.to_json f)
       ; Some ("SnapshotId", String.to_json v.snapshot_id)
       ])

let of_json j =
  { snapshot_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotId"))
  ; storage_tier =
      Aws.Util.option_map (Aws.Json.lookup j "StorageTier") TargetStorageTier.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
