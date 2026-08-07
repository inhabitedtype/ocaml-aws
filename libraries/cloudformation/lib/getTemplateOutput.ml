open Aws.BaseTypes

type t =
  { template_body : String.t option
  ; stages_available : StageList.t
  }

let make ?template_body ?(stages_available = []) () = { template_body; stages_available }

let parse xml =
  Some
    { template_body =
        Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
    ; stages_available =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StagesAvailable" xml) StageList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("StagesAvailable.member", StageList.to_query v.stages_available))
       ; Aws.Util.option_map v.template_body (fun f ->
             Aws.Query.Pair ("TemplateBody", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StagesAvailable", StageList.to_json v.stages_available)
       ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
       ])

let of_json j =
  { template_body = Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
  ; stages_available =
      StageList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StagesAvailable"))
  }
