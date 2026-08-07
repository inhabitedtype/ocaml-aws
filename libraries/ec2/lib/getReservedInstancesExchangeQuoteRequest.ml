open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; reserved_instance_ids : ReservedInstanceIdSet.t
  ; target_configurations : TargetConfigurationRequestSet.t
  }

let make ?dry_run ~reserved_instance_ids ?(target_configurations = []) () =
  { dry_run; reserved_instance_ids; target_configurations }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; reserved_instance_ids =
        Aws.Xml.required
          "ReservedInstanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstanceId" xml)
             ReservedInstanceIdSet.parse)
    ; target_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetConfiguration" xml)
             TargetConfigurationRequestSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TargetConfiguration"
              , TargetConfigurationRequestSet.to_query v.target_configurations ))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstanceId"
              , ReservedInstanceIdSet.to_query v.reserved_instance_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "TargetConfiguration"
           , TargetConfigurationRequestSet.to_json v.target_configurations )
       ; Some ("ReservedInstanceId", ReservedInstanceIdSet.to_json v.reserved_instance_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; reserved_instance_ids =
      ReservedInstanceIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedInstanceId"))
  ; target_configurations =
      TargetConfigurationRequestSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetConfiguration"))
  }
