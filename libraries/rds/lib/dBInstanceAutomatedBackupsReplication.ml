open Aws.BaseTypes

type t = { d_b_instance_automated_backups_arn : String.t option }

let make ?d_b_instance_automated_backups_arn () = { d_b_instance_automated_backups_arn }

let parse xml =
  Some
    { d_b_instance_automated_backups_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "DBInstanceAutomatedBackupsArn" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance_automated_backups_arn (fun f ->
             Aws.Query.Pair ("DBInstanceAutomatedBackupsArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance_automated_backups_arn (fun f ->
             "DBInstanceAutomatedBackupsArn", String.to_json f)
       ])

let of_json j =
  { d_b_instance_automated_backups_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBInstanceAutomatedBackupsArn")
        String.of_json
  }
