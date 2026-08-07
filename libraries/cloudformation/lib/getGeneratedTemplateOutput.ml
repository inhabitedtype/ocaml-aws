open Aws.BaseTypes

type t =
  { status : GeneratedTemplateStatus.t option
  ; template_body : String.t option
  }

let make ?status ?template_body () = { status; template_body }

let parse xml =
  Some
    { status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) GeneratedTemplateStatus.parse
    ; template_body =
        Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", GeneratedTemplateStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "Status", GeneratedTemplateStatus.to_json f)
       ])

let of_json j =
  { status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") GeneratedTemplateStatus.of_json
  ; template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  }
