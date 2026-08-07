open Aws.BaseTypes

type t =
  { dbi_resource_id : String.t option
  ; d_b_instance_automated_backups_arn : String.t option
  }

let make ?dbi_resource_id ?d_b_instance_automated_backups_arn () =
  { dbi_resource_id; d_b_instance_automated_backups_arn }

let parse xml =
  Some
    { dbi_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbiResourceId" xml) String.parse
    ; d_b_instance_automated_backups_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "DBInstanceAutomatedBackupsArn" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance_automated_backups_arn (fun f ->
             Aws.Query.Pair ("DBInstanceAutomatedBackupsArn", String.to_query f))
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             Aws.Query.Pair ("DbiResourceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance_automated_backups_arn (fun f ->
             "DBInstanceAutomatedBackupsArn", String.to_json f)
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             "DbiResourceId", String.to_json f)
       ])

let of_json j =
  { dbi_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbiResourceId") String.of_json
  ; d_b_instance_automated_backups_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBInstanceAutomatedBackupsArn")
        String.of_json
  }
