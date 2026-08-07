open Aws.BaseTypes

type t =
  { attribute : SnapshotAttributeName.t
  ; snapshot_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~attribute ~snapshot_id ?dry_run () = { attribute; snapshot_id; dry_run }

let parse xml =
  Some
    { attribute =
        Aws.Xml.required
          "Attribute"
          (Aws.Util.option_bind
             (Aws.Xml.member "Attribute" xml)
             SnapshotAttributeName.parse)
    ; snapshot_id =
        Aws.Xml.required
          "SnapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("SnapshotId", String.to_query v.snapshot_id))
       ; Some (Aws.Query.Pair ("Attribute", SnapshotAttributeName.to_query v.attribute))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("SnapshotId", String.to_json v.snapshot_id)
       ; Some ("Attribute", SnapshotAttributeName.to_json v.attribute)
       ])

let of_json j =
  { attribute =
      SnapshotAttributeName.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Attribute"))
  ; snapshot_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
