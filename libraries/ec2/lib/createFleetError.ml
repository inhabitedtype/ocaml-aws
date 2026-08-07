open Aws.BaseTypes

type t =
  { launch_template_and_overrides : LaunchTemplateAndOverridesResponse.t option
  ; lifecycle : InstanceLifecycle.t option
  ; error_code : String.t option
  ; error_message : String.t option
  }

let make ?launch_template_and_overrides ?lifecycle ?error_code ?error_message () =
  { launch_template_and_overrides; lifecycle; error_code; error_message }

let parse xml =
  Some
    { launch_template_and_overrides =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplateAndOverrides" xml)
          LaunchTemplateAndOverridesResponse.parse
    ; lifecycle =
        Aws.Util.option_bind (Aws.Xml.member "lifecycle" xml) InstanceLifecycle.parse
    ; error_code = Aws.Util.option_bind (Aws.Xml.member "errorCode" xml) String.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "errorMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.error_code (fun f ->
             Aws.Query.Pair ("ErrorCode", String.to_query f))
       ; Aws.Util.option_map v.lifecycle (fun f ->
             Aws.Query.Pair ("Lifecycle", InstanceLifecycle.to_query f))
       ; Aws.Util.option_map v.launch_template_and_overrides (fun f ->
             Aws.Query.Pair
               ( "LaunchTemplateAndOverrides"
               , LaunchTemplateAndOverridesResponse.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f -> "errorMessage", String.to_json f)
       ; Aws.Util.option_map v.error_code (fun f -> "errorCode", String.to_json f)
       ; Aws.Util.option_map v.lifecycle (fun f ->
             "lifecycle", InstanceLifecycle.to_json f)
       ; Aws.Util.option_map v.launch_template_and_overrides (fun f ->
             "launchTemplateAndOverrides", LaunchTemplateAndOverridesResponse.to_json f)
       ])

let of_json j =
  { launch_template_and_overrides =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplateAndOverrides")
        LaunchTemplateAndOverridesResponse.of_json
  ; lifecycle =
      Aws.Util.option_map (Aws.Json.lookup j "lifecycle") InstanceLifecycle.of_json
  ; error_code = Aws.Util.option_map (Aws.Json.lookup j "errorCode") String.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "errorMessage") String.of_json
  }
