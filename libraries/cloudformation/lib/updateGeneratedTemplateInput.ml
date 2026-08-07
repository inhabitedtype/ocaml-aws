open Aws.BaseTypes

type t =
  { generated_template_name : String.t
  ; new_generated_template_name : String.t option
  ; add_resources : ResourceDefinitions.t
  ; remove_resources : JazzLogicalResourceIds.t
  ; refresh_all_resources : Boolean.t option
  ; template_configuration : TemplateConfiguration.t option
  }

let make
    ~generated_template_name
    ?new_generated_template_name
    ?(add_resources = [])
    ?(remove_resources = [])
    ?refresh_all_resources
    ?template_configuration
    () =
  { generated_template_name
  ; new_generated_template_name
  ; add_resources
  ; remove_resources
  ; refresh_all_resources
  ; template_configuration
  }

let parse xml =
  Some
    { generated_template_name =
        Aws.Xml.required
          "GeneratedTemplateName"
          (Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateName" xml) String.parse)
    ; new_generated_template_name =
        Aws.Util.option_bind (Aws.Xml.member "NewGeneratedTemplateName" xml) String.parse
    ; add_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddResources" xml)
             ResourceDefinitions.parse)
    ; remove_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveResources" xml)
             JazzLogicalResourceIds.parse)
    ; refresh_all_resources =
        Aws.Util.option_bind (Aws.Xml.member "RefreshAllResources" xml) Boolean.parse
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
       ; Aws.Util.option_map v.refresh_all_resources (fun f ->
             Aws.Query.Pair ("RefreshAllResources", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RemoveResources.member"
              , JazzLogicalResourceIds.to_query v.remove_resources ))
       ; Some
           (Aws.Query.Pair
              ("AddResources.member", ResourceDefinitions.to_query v.add_resources))
       ; Aws.Util.option_map v.new_generated_template_name (fun f ->
             Aws.Query.Pair ("NewGeneratedTemplateName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("GeneratedTemplateName", String.to_query v.generated_template_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_configuration (fun f ->
             "TemplateConfiguration", TemplateConfiguration.to_json f)
       ; Aws.Util.option_map v.refresh_all_resources (fun f ->
             "RefreshAllResources", Boolean.to_json f)
       ; Some ("RemoveResources", JazzLogicalResourceIds.to_json v.remove_resources)
       ; Some ("AddResources", ResourceDefinitions.to_json v.add_resources)
       ; Aws.Util.option_map v.new_generated_template_name (fun f ->
             "NewGeneratedTemplateName", String.to_json f)
       ; Some ("GeneratedTemplateName", String.to_json v.generated_template_name)
       ])

let of_json j =
  { generated_template_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GeneratedTemplateName"))
  ; new_generated_template_name =
      Aws.Util.option_map (Aws.Json.lookup j "NewGeneratedTemplateName") String.of_json
  ; add_resources =
      ResourceDefinitions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddResources"))
  ; remove_resources =
      JazzLogicalResourceIds.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveResources"))
  ; refresh_all_resources =
      Aws.Util.option_map (Aws.Json.lookup j "RefreshAllResources") Boolean.of_json
  ; template_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "TemplateConfiguration")
        TemplateConfiguration.of_json
  }
