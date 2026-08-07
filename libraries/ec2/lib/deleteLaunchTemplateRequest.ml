open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; launch_template_id : String.t option
  ; launch_template_name : String.t option
  }

let make ?dry_run ?launch_template_id ?launch_template_name () =
  { dry_run; launch_template_id; launch_template_name }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; launch_template_id =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateId" xml) String.parse
    ; launch_template_name =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplateName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.launch_template_name (fun f ->
             Aws.Query.Pair ("LaunchTemplateName", String.to_query f))
       ; Aws.Util.option_map v.launch_template_id (fun f ->
             Aws.Query.Pair ("LaunchTemplateId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.launch_template_name (fun f ->
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
  }
