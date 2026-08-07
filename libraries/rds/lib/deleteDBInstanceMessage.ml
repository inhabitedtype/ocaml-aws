open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; skip_final_snapshot : Boolean.t option
  ; final_d_b_snapshot_identifier : String.t option
  ; delete_automated_backups : Boolean.t option
  }

let make
    ~d_b_instance_identifier
    ?skip_final_snapshot
    ?final_d_b_snapshot_identifier
    ?delete_automated_backups
    () =
  { d_b_instance_identifier
  ; skip_final_snapshot
  ; final_d_b_snapshot_identifier
  ; delete_automated_backups
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; skip_final_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "SkipFinalSnapshot" xml) Boolean.parse
    ; final_d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "FinalDBSnapshotIdentifier" xml) String.parse
    ; delete_automated_backups =
        Aws.Util.option_bind (Aws.Xml.member "DeleteAutomatedBackups" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_automated_backups (fun f ->
             Aws.Query.Pair ("DeleteAutomatedBackups", Boolean.to_query f))
       ; Aws.Util.option_map v.final_d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("FinalDBSnapshotIdentifier", String.to_query f))
       ; Aws.Util.option_map v.skip_final_snapshot (fun f ->
             Aws.Query.Pair ("SkipFinalSnapshot", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_automated_backups (fun f ->
             "DeleteAutomatedBackups", Boolean.to_json f)
       ; Aws.Util.option_map v.final_d_b_snapshot_identifier (fun f ->
             "FinalDBSnapshotIdentifier", String.to_json f)
       ; Aws.Util.option_map v.skip_final_snapshot (fun f ->
             "SkipFinalSnapshot", Boolean.to_json f)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; skip_final_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "SkipFinalSnapshot") Boolean.of_json
  ; final_d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "FinalDBSnapshotIdentifier") String.of_json
  ; delete_automated_backups =
      Aws.Util.option_map (Aws.Json.lookup j "DeleteAutomatedBackups") Boolean.of_json
  }
