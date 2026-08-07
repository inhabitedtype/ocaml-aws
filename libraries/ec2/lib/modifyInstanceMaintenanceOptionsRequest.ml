open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; auto_recovery : InstanceAutoRecoveryState.t option
  ; reboot_migration : InstanceRebootMigrationState.t option
  ; dry_run : Boolean.t option
  }

let make ~instance_id ?auto_recovery ?reboot_migration ?dry_run () =
  { instance_id; auto_recovery; reboot_migration; dry_run }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; auto_recovery =
        Aws.Util.option_bind
          (Aws.Xml.member "AutoRecovery" xml)
          InstanceAutoRecoveryState.parse
    ; reboot_migration =
        Aws.Util.option_bind
          (Aws.Xml.member "RebootMigration" xml)
          InstanceRebootMigrationState.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.reboot_migration (fun f ->
             Aws.Query.Pair ("RebootMigration", InstanceRebootMigrationState.to_query f))
       ; Aws.Util.option_map v.auto_recovery (fun f ->
             Aws.Query.Pair ("AutoRecovery", InstanceAutoRecoveryState.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.reboot_migration (fun f ->
             "RebootMigration", InstanceRebootMigrationState.to_json f)
       ; Aws.Util.option_map v.auto_recovery (fun f ->
             "AutoRecovery", InstanceAutoRecoveryState.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; auto_recovery =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutoRecovery")
        InstanceAutoRecoveryState.of_json
  ; reboot_migration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RebootMigration")
        InstanceRebootMigrationState.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
