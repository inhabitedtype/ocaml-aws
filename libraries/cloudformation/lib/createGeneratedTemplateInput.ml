open Aws.BaseTypes

type t =
  { resources : ResourceDefinitions.t
  ; generated_template_name : String.t
  ; stack_name : String.t option
  ; template_configuration : TemplateConfiguration.t option
  }

let make ?(resources = []) ~generated_template_name ?stack_name ?template_configuration ()
    =
  { resources; generated_template_name; stack_name; template_configuration }

let parse xml =
  Some
    { resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Resources" xml)
             ResourceDefinitions.parse)
    ; generated_template_name =
        Aws.Xml.required
          "GeneratedTemplateName"
          (Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateName" xml) String.parse)
    ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; template_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "TemplateConfiguration" xml)
          TemplateConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_configuration (fun f ->
             Aws.Query.Pair ("TemplateConfiguration", TemplateConfiguration.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("GeneratedTemplateName", String.to_query v.generated_template_name))
       ; Some
           (Aws.Query.Pair ("Resources.member", ResourceDefinitions.to_query v.resources))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_configuration (fun f ->
             "TemplateConfiguration", TemplateConfiguration.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ; Some ("GeneratedTemplateName", String.to_json v.generated_template_name)
       ; Some ("Resources", ResourceDefinitions.to_json v.resources)
       ])

let of_json j =
  { resources =
      ResourceDefinitions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Resources"))
  ; generated_template_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GeneratedTemplateName"))
  ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; template_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "TemplateConfiguration")
        TemplateConfiguration.of_json
  }
