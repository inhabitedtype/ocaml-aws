open Aws.BaseTypes

type t =
  { instance_event_window_id : String.t option
  ; time_ranges : InstanceEventWindowTimeRangeList.t
  ; name : String.t option
  ; cron_expression : String.t option
  ; association_target : InstanceEventWindowAssociationTarget.t option
  ; state : InstanceEventWindowState.t option
  ; tags : TagList.t
  }

let make
    ?instance_event_window_id
    ?(time_ranges = [])
    ?name
    ?cron_expression
    ?association_target
    ?state
    ?(tags = [])
    () =
  { instance_event_window_id
  ; time_ranges
  ; name
  ; cron_expression
  ; association_target
  ; state
  ; tags
  }

let parse xml =
  Some
    { instance_event_window_id =
        Aws.Util.option_bind (Aws.Xml.member "instanceEventWindowId" xml) String.parse
    ; time_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "timeRangeSet" xml)
             InstanceEventWindowTimeRangeList.parse)
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; cron_expression =
        Aws.Util.option_bind (Aws.Xml.member "cronExpression" xml) String.parse
    ; association_target =
        Aws.Util.option_bind
          (Aws.Xml.member "associationTarget" xml)
          InstanceEventWindowAssociationTarget.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) InstanceEventWindowState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", InstanceEventWindowState.to_query f))
       ; Aws.Util.option_map v.association_target (fun f ->
             Aws.Query.Pair
               ("AssociationTarget", InstanceEventWindowAssociationTarget.to_query f))
       ; Aws.Util.option_map v.cron_expression (fun f ->
             Aws.Query.Pair ("CronExpression", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TimeRangeSet", InstanceEventWindowTimeRangeList.to_query v.time_ranges))
       ; Aws.Util.option_map v.instance_event_window_id (fun f ->
             Aws.Query.Pair ("InstanceEventWindowId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f ->
             "state", InstanceEventWindowState.to_json f)
       ; Aws.Util.option_map v.association_target (fun f ->
             "associationTarget", InstanceEventWindowAssociationTarget.to_json f)
       ; Aws.Util.option_map v.cron_expression (fun f ->
             "cronExpression", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Some ("timeRangeSet", InstanceEventWindowTimeRangeList.to_json v.time_ranges)
       ; Aws.Util.option_map v.instance_event_window_id (fun f ->
             "instanceEventWindowId", String.to_json f)
       ])

let of_json j =
  { instance_event_window_id =
      Aws.Util.option_map (Aws.Json.lookup j "instanceEventWindowId") String.of_json
  ; time_ranges =
      InstanceEventWindowTimeRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "timeRangeSet"))
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; cron_expression =
      Aws.Util.option_map (Aws.Json.lookup j "cronExpression") String.of_json
  ; association_target =
      Aws.Util.option_map
        (Aws.Json.lookup j "associationTarget")
        InstanceEventWindowAssociationTarget.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") InstanceEventWindowState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
