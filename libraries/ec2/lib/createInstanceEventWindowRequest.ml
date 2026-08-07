open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; name : String.t option
  ; time_ranges : InstanceEventWindowTimeRangeRequestSet.t
  ; cron_expression : String.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make ?dry_run ?name ?(time_ranges = []) ?cron_expression ?(tag_specifications = []) ()
    =
  { dry_run; name; time_ranges; cron_expression; tag_specifications }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; time_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TimeRange" xml)
             InstanceEventWindowTimeRangeRequestSet.parse)
    ; cron_expression =
        Aws.Util.option_bind (Aws.Xml.member "CronExpression" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.cron_expression (fun f ->
             Aws.Query.Pair ("CronExpression", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TimeRange", InstanceEventWindowTimeRangeRequestSet.to_query v.time_ranges))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.cron_expression (fun f ->
             "CronExpression", String.to_json f)
       ; Some ("TimeRange", InstanceEventWindowTimeRangeRequestSet.to_json v.time_ranges)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; time_ranges =
      InstanceEventWindowTimeRangeRequestSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TimeRange"))
  ; cron_expression =
      Aws.Util.option_map (Aws.Json.lookup j "CronExpression") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
