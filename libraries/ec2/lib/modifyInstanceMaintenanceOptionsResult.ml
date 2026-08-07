open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; auto_recovery : InstanceAutoRecoveryState.t option
  ; reboot_migration : InstanceRebootMigrationState.t option
  }

let make ?instance_id ?auto_recovery ?reboot_migration () =
  { instance_id; auto_recovery; reboot_migration }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; auto_recovery =
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
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reboot_migration (fun f ->
             "rebootMigration", InstanceRebootMigrationState.to_json f)
       ; Aws.Util.option_map v.auto_recovery (fun f ->
             "autoRecovery", InstanceAutoRecoveryState.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; auto_recovery =
      Aws.Util.option_map
        (Aws.Json.lookup j "autoRecovery")
        InstanceAutoRecoveryState.of_json
  ; reboot_migration =
      Aws.Util.option_map
        (Aws.Json.lookup j "rebootMigration")
        InstanceRebootMigrationState.of_json
  }
