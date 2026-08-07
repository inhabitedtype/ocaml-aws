open Aws.BaseTypes

type t = { generated_template_id : String.t option }

let make ?generated_template_id () = { generated_template_id }

let parse xml =
  Some
    { generated_template_id =
        Aws.Util.option_bind (Aws.Xml.member "GeneratedTemplateId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.generated_template_id (fun f ->
             Aws.Query.Pair ("GeneratedTemplateId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.generated_template_id (fun f ->
             "GeneratedTemplateId", String.to_json f)
       ])

let of_json j =
  { generated_template_id =
      Aws.Util.option_map (Aws.Json.lookup j "GeneratedTemplateId") String.of_json
  }
