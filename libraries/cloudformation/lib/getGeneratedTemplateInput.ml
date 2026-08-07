open Aws.BaseTypes

type t =
  { format : TemplateFormat.t option
  ; generated_template_name : String.t
  }

let make ?format ~generated_template_name () = { format; generated_template_name }

let parse xml =
  Some
    { format = Aws.Util.option_bind (Aws.Xml.member "Format" xml) TemplateFormat.parse
    ; generated_template_name =
        Aws.Xml.required
          "GeneratedTemplateName"
          (Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("GeneratedTemplateName", String.to_query v.generated_template_name))
       ; Aws.Util.option_map v.format (fun f ->
             Aws.Query.Pair ("Format", TemplateFormat.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("GeneratedTemplateName", String.to_json v.generated_template_name)
       ; Aws.Util.option_map v.format (fun f -> "Format", TemplateFormat.to_json f)
       ])

let of_json j =
  { format = Aws.Util.option_map (Aws.Json.lookup j "Format") TemplateFormat.of_json
  ; generated_template_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GeneratedTemplateName"))
  }
