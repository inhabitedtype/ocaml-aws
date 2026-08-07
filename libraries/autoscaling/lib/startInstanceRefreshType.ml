open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; strategy : RefreshStrategy.t option
  ; desired_configuration : DesiredConfiguration.t option
  ; preferences : RefreshPreferences.t option
  }

let make ~auto_scaling_group_name ?strategy ?desired_configuration ?preferences () =
  { auto_scaling_group_name; strategy; desired_configuration; preferences }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; strategy =
        Aws.Util.option_bind (Aws.Xml.member "Strategy" xml) RefreshStrategy.parse
    ; desired_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "DesiredConfiguration" xml)
          DesiredConfiguration.parse
    ; preferences =
        Aws.Util.option_bind (Aws.Xml.member "Preferences" xml) RefreshPreferences.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.preferences (fun f ->
             Aws.Query.Pair ("Preferences", RefreshPreferences.to_query f))
       ; Aws.Util.option_map v.desired_configuration (fun f ->
             Aws.Query.Pair ("DesiredConfiguration", DesiredConfiguration.to_query f))
       ; Aws.Util.option_map v.strategy (fun f ->
             Aws.Query.Pair ("Strategy", RefreshStrategy.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.preferences (fun f ->
             "Preferences", RefreshPreferences.to_json f)
       ; Aws.Util.option_map v.desired_configuration (fun f ->
             "DesiredConfiguration", DesiredConfiguration.to_json f)
       ; Aws.Util.option_map v.strategy (fun f -> "Strategy", RefreshStrategy.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; strategy = Aws.Util.option_map (Aws.Json.lookup j "Strategy") RefreshStrategy.of_json
  ; desired_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "DesiredConfiguration")
        DesiredConfiguration.of_json
  ; preferences =
      Aws.Util.option_map (Aws.Json.lookup j "Preferences") RefreshPreferences.of_json
  }
