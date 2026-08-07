open Aws.BaseTypes

type t =
  { dashboard_id : String.t
  ; widgets : RequestWidgetList.t
  ; refresh_schedule : RefreshSchedule.t option
  ; termination_protection_enabled : Boolean.t option
  }

let make
    ~dashboard_id
    ?(widgets = [])
    ?refresh_schedule
    ?termination_protection_enabled
    () =
  { dashboard_id; widgets; refresh_schedule; termination_protection_enabled }

let parse xml =
  Some
    { dashboard_id =
        Aws.Xml.required
          "DashboardId"
          (Aws.Util.option_bind (Aws.Xml.member "DashboardId" xml) String.parse)
    ; widgets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Widgets" xml) RequestWidgetList.parse)
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
       ; Some (Aws.Query.Pair ("Widgets.member", RequestWidgetList.to_query v.widgets))
       ; Some (Aws.Query.Pair ("DashboardId", String.to_query v.dashboard_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.termination_protection_enabled (fun f ->
             "TerminationProtectionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.refresh_schedule (fun f ->
             "RefreshSchedule", RefreshSchedule.to_json f)
       ; Some ("Widgets", RequestWidgetList.to_json v.widgets)
       ; Some ("DashboardId", String.to_json v.dashboard_id)
       ])

let of_json j =
  { dashboard_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardId"))
  ; widgets =
      RequestWidgetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Widgets"))
  ; refresh_schedule =
      Aws.Util.option_map (Aws.Json.lookup j "RefreshSchedule") RefreshSchedule.of_json
  ; termination_protection_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "TerminationProtectionEnabled")
        Boolean.of_json
  }
