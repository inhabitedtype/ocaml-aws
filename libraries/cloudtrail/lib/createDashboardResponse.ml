open Aws.BaseTypes

type t =
  { dashboard_arn : String.t option
  ; name : String.t option
  ; type_ : DashboardType.t option
  ; widgets : WidgetList.t
  ; tags_list : TagsList.t
  ; refresh_schedule : RefreshSchedule.t option
  ; termination_protection_enabled : Boolean.t option
  }

let make
    ?dashboard_arn
    ?name
    ?type_
    ?(widgets = [])
    ?(tags_list = [])
    ?refresh_schedule
    ?termination_protection_enabled
    () =
  { dashboard_arn
  ; name
  ; type_
  ; widgets
  ; tags_list
  ; refresh_schedule
  ; termination_protection_enabled
  }

let parse xml =
  Some
    { dashboard_arn =
        Aws.Util.option_bind (Aws.Xml.member "DashboardArn" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) DashboardType.parse
    ; widgets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Widgets" xml) WidgetList.parse)
    ; tags_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagsList" xml) TagsList.parse)
    ; refresh_schedule =
        Aws.Util.option_bind (Aws.Xml.member "RefreshSchedule" xml) RefreshSchedule.parse
    ; termination_protection_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "TerminationProtectionEnabled" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.termination_protection_enabled (fun f ->
             Aws.Query.Pair ("TerminationProtectionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.refresh_schedule (fun f ->
             Aws.Query.Pair ("RefreshSchedule", RefreshSchedule.to_query f))
       ; Some (Aws.Query.Pair ("TagsList.member", TagsList.to_query v.tags_list))
       ; Some (Aws.Query.Pair ("Widgets.member", WidgetList.to_query v.widgets))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", DashboardType.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.dashboard_arn (fun f ->
             Aws.Query.Pair ("DashboardArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.termination_protection_enabled (fun f ->
             "TerminationProtectionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.refresh_schedule (fun f ->
             "RefreshSchedule", RefreshSchedule.to_json f)
       ; Some ("TagsList", TagsList.to_json v.tags_list)
       ; Some ("Widgets", WidgetList.to_json v.widgets)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", DashboardType.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.dashboard_arn (fun f -> "DashboardArn", String.to_json f)
       ])

let of_json j =
  { dashboard_arn = Aws.Util.option_map (Aws.Json.lookup j "DashboardArn") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") DashboardType.of_json
  ; widgets = WidgetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Widgets"))
  ; tags_list = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagsList"))
  ; refresh_schedule =
      Aws.Util.option_map (Aws.Json.lookup j "RefreshSchedule") RefreshSchedule.of_json
  ; termination_protection_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "TerminationProtectionEnabled")
        Boolean.of_json
  }
