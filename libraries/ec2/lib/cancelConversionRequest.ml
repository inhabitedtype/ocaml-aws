open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; conversion_task_id : String.t
  ; reason_message : String.t option
  }

let make ?dry_run ~conversion_task_id ?reason_message () =
  { dry_run; conversion_task_id; reason_message }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; conversion_task_id =
        Aws.Xml.required
          "conversionTaskId"
          (Aws.Util.option_bind (Aws.Xml.member "conversionTaskId" xml) String.parse)
    ; reason_message =
        Aws.Util.option_bind (Aws.Xml.member "reasonMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reason_message (fun f ->
             Aws.Query.Pair ("ReasonMessage", String.to_query f))
       ; Some (Aws.Query.Pair ("ConversionTaskId", String.to_query v.conversion_task_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reason_message (fun f -> "reasonMessage", String.to_json f)
       ; Some ("conversionTaskId", String.to_json v.conversion_task_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; conversion_task_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "conversionTaskId"))
  ; reason_message =
      Aws.Util.option_map (Aws.Json.lookup j "reasonMessage") String.of_json
  }
