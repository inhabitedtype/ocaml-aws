open Aws.BaseTypes

type t = { generated_template_name : String.t }

let make ~generated_template_name () = { generated_template_name }

let parse xml =
  Some
    { generated_template_name =
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
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("GeneratedTemplateName", String.to_json v.generated_template_name) ])

let of_json j =
  { generated_template_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GeneratedTemplateName"))
  }
