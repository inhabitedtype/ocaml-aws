open Aws.BaseTypes

type t =
  { d_b_snapshot_identifier : String.t
  ; engine_version : String.t option
  ; option_group_name : String.t option
  }

let make ~d_b_snapshot_identifier ?engine_version ?option_group_name () =
  { d_b_snapshot_identifier; engine_version; option_group_name }

let parse xml =
  Some
    { d_b_snapshot_identifier =
        Aws.Xml.required
          "DBSnapshotIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse)
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Some ("DBSnapshotIdentifier", String.to_json v.d_b_snapshot_identifier)
       ])

let of_json j =
  { d_b_snapshot_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSnapshotIdentifier"))
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  }
