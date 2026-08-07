open Aws.BaseTypes

type t =
  { stack_name : String.t option
  ; change_set_name : String.t option
  ; template_stage : TemplateStage.t option
  }

let make ?stack_name ?change_set_name ?template_stage () =
  { stack_name; change_set_name; template_stage }

let parse xml =
  Some
    { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; change_set_name =
        Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
    ; template_stage =
        Aws.Util.option_bind (Aws.Xml.member "TemplateStage" xml) TemplateStage.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_stage (fun f ->
             Aws.Query.Pair ("TemplateStage", TemplateStage.to_query f))
       ; Aws.Util.option_map v.change_set_name (fun f ->
             Aws.Query.Pair ("ChangeSetName", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_stage (fun f ->
             "TemplateStage", TemplateStage.to_json f)
       ; Aws.Util.option_map v.change_set_name (fun f ->
             "ChangeSetName", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ])

let of_json j =
  { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; change_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
  ; template_stage =
      Aws.Util.option_map (Aws.Json.lookup j "TemplateStage") TemplateStage.of_json
  }
