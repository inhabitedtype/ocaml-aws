open Aws.BaseTypes

type t =
  { template_body : String.t option
  ; template_u_r_l : String.t option
  }

let make ?template_body ?template_u_r_l () = { template_body; template_u_r_l }

let parse xml =
  Some
    { template_body =
        Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
    ; template_u_r_l =
        Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_u_r_l (fun f ->
             Aws.Query.Pair ("TemplateURL", String.to_query f))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ])

let of_json j =
  { template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; template_u_r_l = Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
  }
