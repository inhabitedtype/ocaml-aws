type t = { slot_migration : SlotMigration.t option }

let make ?slot_migration () = { slot_migration }

let parse xml =
  Some
    { slot_migration =
        Aws.Util.option_bind (Aws.Xml.member "SlotMigration" xml) SlotMigration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.slot_migration (fun f ->
             Aws.Query.Pair ("SlotMigration", SlotMigration.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.slot_migration (fun f ->
             "SlotMigration", SlotMigration.to_json f)
       ])

let of_json j =
  { slot_migration =
      Aws.Util.option_map (Aws.Json.lookup j "SlotMigration") SlotMigration.of_json
  }
