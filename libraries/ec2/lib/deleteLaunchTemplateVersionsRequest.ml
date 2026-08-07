open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; launch_template_id : String.t option
  ; launch_template_name : String.t option
  ; versions : VersionStringList.t
  }

let make ?dry_run ?launch_template_id ?launch_template_name ~versions () =
  { dry_run; launch_template_id; launch_template_name; versions }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateName" xml) String.parse
    ; versions =
        Aws.Xml.required
          "LaunchTemplateVersion"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateVersion" xml)
             VersionStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("LaunchTemplateVersion", VersionStringList.to_query v.versions))
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LaunchTemplateVersion", VersionStringList.to_json v.versions)
       ; Aws.Util.option_map v.launch_template_name (fun f ->
             "LaunchTemplateName", String.to_json f)
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             "LaunchTemplateId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; launch_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateId") String.of_json
  ; launch_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplateName") String.of_json
  ; versions =
      VersionStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateVersion"))
  }
