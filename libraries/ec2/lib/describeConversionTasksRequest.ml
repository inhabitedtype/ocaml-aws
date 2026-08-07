open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; conversion_task_ids : ConversionIdStringList.t
  }

let make ?dry_run ?(conversion_task_ids = []) () = { dry_run; conversion_task_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; conversion_task_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "conversionTaskId" xml)
             ConversionIdStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ConversionTaskId", ConversionIdStringList.to_query v.conversion_task_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("conversionTaskId", ConversionIdStringList.to_json v.conversion_task_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; conversion_task_ids =
      ConversionIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "conversionTaskId"))
  }
