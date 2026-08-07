open Aws.BaseTypes

type t =
  { instance_ids : InstanceIdStringList.t
  ; force : Boolean.t option
  ; skip_os_shutdown : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make ~instance_ids ?force ?skip_os_shutdown ?dry_run () =
  { instance_ids; force; skip_os_shutdown; dry_run }

let parse xml =
  Some
    { instance_ids =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceId" xml)
             InstanceIdStringList.parse)
    ; force = Aws.Util.option_bind (Aws.Xml.member "Force" xml) Boolean.parse
    ; skip_os_shutdown =
        Aws.Util.option_bind (Aws.Xml.member "SkipOsShutdown" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.skip_os_shutdown (fun f ->
             Aws.Query.Pair ("SkipOsShutdown", Boolean.to_query f))
       ; Aws.Util.option_map v.force (fun f ->
             Aws.Query.Pair ("Force", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("InstanceId", InstanceIdStringList.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.skip_os_shutdown (fun f ->
             "SkipOsShutdown", Boolean.to_json f)
       ; Aws.Util.option_map v.force (fun f -> "Force", Boolean.to_json f)
       ; Some ("InstanceId", InstanceIdStringList.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; force = Aws.Util.option_map (Aws.Json.lookup j "Force") Boolean.of_json
  ; skip_os_shutdown =
      Aws.Util.option_map (Aws.Json.lookup j "SkipOsShutdown") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
