open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; name : String.t option
  ; instance_event_window_id : String.t
  ; time_ranges : InstanceEventWindowTimeRangeRequestSet.t
  ; cron_expression : String.t option
  }

let make ?dry_run ?name ~instance_event_window_id ?(time_ranges = []) ?cron_expression ()
    =
  { dry_run; name; instance_event_window_id; time_ranges; cron_expression }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; instance_event_window_id =
        Aws.Xml.required
          "InstanceEventWindowId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceEventWindowId" xml) String.parse)
    ; time_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TimeRange" xml)
             InstanceEventWindowTimeRangeRequestSet.parse)
    ; cron_expression =
        Aws.Util.option_bind (Aws.Xml.member "CronExpression" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cron_expression (fun f ->
             Aws.Query.Pair ("CronExpression", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TimeRange", InstanceEventWindowTimeRangeRequestSet.to_query v.time_ranges))
       ; Some
           (Aws.Query.Pair
              ("InstanceEventWindowId", String.to_query v.instance_event_window_id))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cron_expression (fun f ->
             "CronExpression", String.to_json f)
       ; Some ("TimeRange", InstanceEventWindowTimeRangeRequestSet.to_json v.time_ranges)
       ; Some ("InstanceEventWindowId", String.to_json v.instance_event_window_id)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; instance_event_window_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceEventWindowId"))
  ; time_ranges =
      InstanceEventWindowTimeRangeRequestSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TimeRange"))
  ; cron_expression =
      Aws.Util.option_map (Aws.Json.lookup j "CronExpression") String.of_json
  }
