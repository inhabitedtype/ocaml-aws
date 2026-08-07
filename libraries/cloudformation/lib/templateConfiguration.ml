type t =
  { deletion_policy : GeneratedTemplateDeletionPolicy.t option
  ; update_replace_policy : GeneratedTemplateUpdateReplacePolicy.t option
  }

let make ?deletion_policy ?update_replace_policy () =
  { deletion_policy; update_replace_policy }

let parse xml =
  Some
    { deletion_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "DeletionPolicy" xml)
          GeneratedTemplateDeletionPolicy.parse
    ; update_replace_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "UpdateReplacePolicy" xml)
          GeneratedTemplateUpdateReplacePolicy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_replace_policy (fun f ->
             Aws.Query.Pair
               ("UpdateReplacePolicy", GeneratedTemplateUpdateReplacePolicy.to_query f))
       ; Aws.Util.option_map v.deletion_policy (fun f ->
             Aws.Query.Pair ("DeletionPolicy", GeneratedTemplateDeletionPolicy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_replace_policy (fun f ->
             "UpdateReplacePolicy", GeneratedTemplateUpdateReplacePolicy.to_json f)
       ; Aws.Util.option_map v.deletion_policy (fun f ->
             "DeletionPolicy", GeneratedTemplateDeletionPolicy.to_json f)
       ])

let of_json j =
  { deletion_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeletionPolicy")
        GeneratedTemplateDeletionPolicy.of_json
  ; update_replace_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "UpdateReplacePolicy")
        GeneratedTemplateUpdateReplacePolicy.of_json
  }
