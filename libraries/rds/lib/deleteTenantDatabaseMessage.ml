open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; tenant_d_b_name : String.t
  ; skip_final_snapshot : Boolean.t option
  ; final_d_b_snapshot_identifier : String.t option
  }

let make
    ~d_b_instance_identifier
    ~tenant_d_b_name
    ?skip_final_snapshot
    ?final_d_b_snapshot_identifier
    () =
  { d_b_instance_identifier
  ; tenant_d_b_name
  ; skip_final_snapshot
  ; final_d_b_snapshot_identifier
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; tenant_d_b_name =
        Aws.Xml.required
          "TenantDBName"
          (Aws.Util.option_bind (Aws.Xml.member "TenantDBName" xml) String.parse)
    ; skip_final_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "SkipFinalSnapshot" xml) Boolean.parse
    ; final_d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "FinalDBSnapshotIdentifier" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("FinalDBSnapshotIdentifier", String.to_query f))
       ; Aws.Util.option_map v.skip_final_snapshot (fun f ->
             Aws.Query.Pair ("SkipFinalSnapshot", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("TenantDBName", String.to_query v.tenant_d_b_name))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_d_b_snapshot_identifier (fun f ->
             "FinalDBSnapshotIdentifier", String.to_json f)
       ; Aws.Util.option_map v.skip_final_snapshot (fun f ->
             "SkipFinalSnapshot", Boolean.to_json f)
       ; Some ("TenantDBName", String.to_json v.tenant_d_b_name)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; tenant_d_b_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TenantDBName"))
  ; skip_final_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "SkipFinalSnapshot") Boolean.of_json
  ; final_d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "FinalDBSnapshotIdentifier") String.of_json
  }
