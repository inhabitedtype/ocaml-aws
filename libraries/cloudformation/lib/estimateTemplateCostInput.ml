open Aws.BaseTypes

type t =
  { template_body : String.t option
  ; template_u_r_l : String.t option
  ; parameters : Parameters.t
  }

let make ?template_body ?template_u_r_l ?(parameters = []) () =
  { template_body; template_u_r_l; parameters }

let parse xml =
  Some
    { template_body =
        Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
    ; template_u_r_l =
        Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
       ; Aws.Util.option_map v.template_u_r_l (fun f ->
             Aws.Query.Pair ("TemplateURL", String.to_query f))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Parameters", Parameters.to_json v.parameters)
       ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ])

let of_json j =
  { template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; template_u_r_l = Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
  ; parameters =
      Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  }
