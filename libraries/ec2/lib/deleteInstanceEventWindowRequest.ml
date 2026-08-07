open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; force_delete : Boolean.t option
  ; instance_event_window_id : String.t
  }

let make ?dry_run ?force_delete ~instance_event_window_id () =
  { dry_run; force_delete; instance_event_window_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; force_delete = Aws.Util.option_bind (Aws.Xml.member "ForceDelete" xml) Boolean.parse
    ; instance_event_window_id =
        Aws.Xml.required
          "InstanceEventWindowId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceEventWindowId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("InstanceEventWindowId", String.to_query v.instance_event_window_id))
       ; Aws.Util.option_map v.force_delete (fun f ->
             Aws.Query.Pair ("ForceDelete", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("InstanceEventWindowId", String.to_json v.instance_event_window_id)
       ; Aws.Util.option_map v.force_delete (fun f -> "ForceDelete", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; force_delete = Aws.Util.option_map (Aws.Json.lookup j "ForceDelete") Boolean.of_json
  ; instance_event_window_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceEventWindowId"))
  }
