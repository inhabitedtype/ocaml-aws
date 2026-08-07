open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; reserved_instances_ids : DeleteQueuedReservedInstancesIdList.t
  }

let make ?dry_run ~reserved_instances_ids () = { dry_run; reserved_instances_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; reserved_instances_ids =
        Aws.Xml.required
          "ReservedInstancesId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstancesId" xml)
             DeleteQueuedReservedInstancesIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedInstancesId"
              , DeleteQueuedReservedInstancesIdList.to_query v.reserved_instances_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ReservedInstancesId"
           , DeleteQueuedReservedInstancesIdList.to_json v.reserved_instances_ids )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; reserved_instances_ids =
      DeleteQueuedReservedInstancesIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedInstancesId"))
  }
