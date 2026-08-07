type t = { d_b_cluster_automated_backup : DBClusterAutomatedBackup.t option }

let make ?d_b_cluster_automated_backup () = { d_b_cluster_automated_backup }

let parse xml =
  Some
    { d_b_cluster_automated_backup =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterAutomatedBackup" xml)
          DBClusterAutomatedBackup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_automated_backup (fun f ->
             Aws.Query.Pair
               ("DBClusterAutomatedBackup", DBClusterAutomatedBackup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_automated_backup (fun f ->
             "DBClusterAutomatedBackup", DBClusterAutomatedBackup.to_json f)
       ])

let of_json j =
  { d_b_cluster_automated_backup =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBClusterAutomatedBackup")
        DBClusterAutomatedBackup.of_json
  }
