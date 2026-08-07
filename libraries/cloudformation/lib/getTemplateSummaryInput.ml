open Aws.BaseTypes

type t =
  { template_body : String.t option
  ; template_u_r_l : String.t option
  ; stack_name : String.t option
  ; stack_set_name : String.t option
  ; call_as : CallAs.t option
  ; template_summary_config : TemplateSummaryConfig.t option
  }

let make
    ?template_body
    ?template_u_r_l
    ?stack_name
    ?stack_set_name
    ?call_as
    ?template_summary_config
    () =
  { template_body
  ; template_u_r_l
  ; stack_name
  ; stack_set_name
  ; call_as
  ; template_summary_config
  }

let parse xml =
  Some
    { template_body =
        Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
    ; template_u_r_l =
        Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
    ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; stack_set_name =
        Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    ; template_summary_config =
        Aws.Util.option_bind
          (Aws.Xml.member "TemplateSummaryConfig" xml)
          TemplateSummaryConfig.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_summary_config (fun f ->
             Aws.Query.Pair ("TemplateSummaryConfig", TemplateSummaryConfig.to_query f))
       ; Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Aws.Util.option_map v.stack_set_name (fun f ->
             Aws.Query.Pair ("StackSetName", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ; Aws.Util.option_map v.template_u_r_l (fun f ->
             Aws.Query.Pair ("TemplateURL", String.to_query f))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_summary_config (fun f ->
             "TemplateSummaryConfig", TemplateSummaryConfig.to_json f)
       ; Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Aws.Util.option_map v.stack_set_name (fun f -> "StackSetName", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ])

let of_json j =
  { template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; template_u_r_l = Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
  ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; stack_set_name = Aws.Util.option_map (Aws.Json.lookup j "StackSetName") String.of_json
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  ; template_summary_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "TemplateSummaryConfig")
        TemplateSummaryConfig.of_json
  }
