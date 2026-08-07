open Aws.BaseTypes

type t =
  { state : SnapshotBlockPublicAccessState.t
  ; dry_run : Boolean.t option
  }

let make ~state ?dry_run () = { state; dry_run }

let parse xml =
  Some
    { state =
        Aws.Xml.required
          "State"
          (Aws.Util.option_bind
             (Aws.Xml.member "State" xml)
             SnapshotBlockPublicAccessState.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("State", SnapshotBlockPublicAccessState.to_query v.state))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("State", SnapshotBlockPublicAccessState.to_json v.state)
       ])

let of_json j =
  { state =
      SnapshotBlockPublicAccessState.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "State"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
