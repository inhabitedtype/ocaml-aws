open Aws.BaseTypes

type t =
  { launch_template_id : String.t option
  ; launch_template_name : String.t option
  ; version : String.t
  }

let make ?launch_template_id ?launch_template_name ~version () =
  { launch_template_id; launch_template_name; version }

let parse xml =
  Some
    { launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateName" xml) String.parse
    ; version =
        Aws.Xml.required
          "Version"
          (Aws.Util.option_bind (Aws.Xml.member "Version" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Version", String.to_query v.version))
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Version", String.to_json v.version)
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             "LaunchTemplateName", String.to_json f)
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             "LaunchTemplateId", String.to_json f)
       ])

let of_json j =
  { launch_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateId") String.of_json
  ; launch_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateName") String.of_json
  ; version = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Version"))
  }
