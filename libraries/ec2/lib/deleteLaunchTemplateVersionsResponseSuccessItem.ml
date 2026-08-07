open Aws.BaseTypes

type t =
  { launch_template_id : String.t option
  ; launch_template_name : String.t option
  ; version_number : Long.t option
  }

let make ?launch_template_id ?launch_template_name ?version_number () =
  { launch_template_id; launch_template_name; version_number }

let parse xml =
  Some
    { launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplateName" xml) String.parse
    ; version_number =
        Aws.Util.option_bind (Aws.Xml.member "versionNumber" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version_number (fun f ->
             Aws.Query.Pair ("VersionNumber", Long.to_query f))
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version_number (fun f -> "versionNumber", Long.to_json f)
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             "launchTemplateName", String.to_json f)
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             "launchTemplateId", String.to_json f)
       ])

let of_json j =
  { launch_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "launchTemplateId") String.of_json
  ; launch_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "launchTemplateName") String.of_json
  ; version_number = Aws.Util.option_map (Aws.Json.lookup j "versionNumber") Long.of_json
  }
