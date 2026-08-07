type t =
  { auto_recovery : InstanceAutoRecoveryState.t option
  ; reboot_migration : InstanceRebootMigrationState.t option
  }

let make ?auto_recovery ?reboot_migration () = { auto_recovery; reboot_migration }

let parse xml =
  Some
    { auto_recovery =
        Aws.Util.option_bind
          (Aws.Xml.member "autoRecovery" xml)
          InstanceAutoRecoveryState.parse
    ; reboot_migration =
        Aws.Util.option_bind
          (Aws.Xml.member "rebootMigration" xml)
          InstanceRebootMigrationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reboot_migration (fun f ->
             Aws.Query.Pair ("RebootMigration", InstanceRebootMigrationState.to_query f))
       ; Aws.Util.option_map v.auto_recovery (fun f ->
             Aws.Query.Pair ("AutoRecovery", InstanceAutoRecoveryState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reboot_migration (fun f ->
             "rebootMigration", InstanceRebootMigrationState.to_json f)
       ; Aws.Util.option_map v.auto_recovery (fun f ->
             "autoRecovery", InstanceAutoRecoveryState.to_json f)
       ])

let of_json j =
  { auto_recovery =
      Aws.Util.option_map
        (Aws.Json.lookup j "autoRecovery")
        InstanceAutoRecoveryState.of_json
  ; reboot_migration =
      Aws.Util.option_map
        (Aws.Json.lookup j "rebootMigration")
        InstanceRebootMigrationState.of_json
  }
