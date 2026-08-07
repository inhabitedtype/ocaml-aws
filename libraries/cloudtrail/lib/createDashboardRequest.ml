open Aws.BaseTypes

type t =
  { name : String.t
  ; refresh_schedule : RefreshSchedule.t option
  ; tags_list : TagsList.t
  ; termination_protection_enabled : Boolean.t option
  ; widgets : RequestWidgetList.t
  }

let make
    ~name
    ?refresh_schedule
    ?(tags_list = [])
    ?termination_protection_enabled
    ?(widgets = [])
    () =
  { name; refresh_schedule; tags_list; termination_protection_enabled; widgets }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; refresh_schedule =
        Aws.Util.option_bind (Aws.Xml.member "RefreshSchedule" xml) RefreshSchedule.parse
    ; tags_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagsList" xml) TagsList.parse)
    ; termination_protection_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "TerminationProtectionEnabled" xml)
          Boolean.parse
    ; widgets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Widgets" xml) RequestWidgetList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Widgets.member", RequestWidgetList.to_query v.widgets))
       ; Aws.Util.option_map v.termination_protection_enabled (fun f ->
             Aws.Query.Pair ("TerminationProtectionEnabled", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("TagsList.member", TagsList.to_query v.tags_list))
       ; Aws.Util.option_map v.refresh_schedule (fun f ->
             Aws.Query.Pair ("RefreshSchedule", RefreshSchedule.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Widgets", RequestWidgetList.to_json v.widgets)
       ; Aws.Util.option_map v.termination_protection_enabled (fun f ->
             "TerminationProtectionEnabled", Boolean.to_json f)
       ; Some ("TagsList", TagsList.to_json v.tags_list)
       ; Aws.Util.option_map v.refresh_schedule (fun f ->
             "RefreshSchedule", RefreshSchedule.to_json f)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; refresh_schedule =
      Aws.Util.option_map (Aws.Json.lookup j "RefreshSchedule") RefreshSchedule.of_json
  ; tags_list = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagsList"))
  ; termination_protection_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "TerminationProtectionEnabled")
        Boolean.of_json
  ; widgets =
      RequestWidgetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Widgets"))
  }
