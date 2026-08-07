type t = { d_b_instance_automated_backup : DBInstanceAutomatedBackup.t option }

let make ?d_b_instance_automated_backup () = { d_b_instance_automated_backup }

let parse xml =
  Some
    { d_b_instance_automated_backup =
        Aws.Util.option_bind
          (Aws.Xml.member "DBInstanceAutomatedBackup" xml)
          DBInstanceAutomatedBackup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance_automated_backup (fun f ->
             Aws.Query.Pair
               ("DBInstanceAutomatedBackup", DBInstanceAutomatedBackup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_instance_automated_backup (fun f ->
             "DBInstanceAutomatedBackup", DBInstanceAutomatedBackup.to_json f)
       ])

let of_json j =
  { d_b_instance_automated_backup =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBInstanceAutomatedBackup")
        DBInstanceAutomatedBackup.of_json
  }
