open Aws.BaseTypes

type t =
  { template_name : String.t
  ; resource_a_r_n : String.t
  ; tags : TagList.t
  }

let make ~template_name ~resource_a_r_n ?(tags = []) () =
  { template_name; resource_a_r_n; tags }

let parse xml =
  Some
    { template_name =
        Aws.Xml.required
          "TemplateName"
          (Aws.Util.option_bind (Aws.Xml.member "TemplateName" xml) String.parse)
    ; resource_a_r_n =
        Aws.Xml.required
          "ResourceARN"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceARN" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("ResourceARN", String.to_query v.resource_a_r_n))
       ; Some (Aws.Query.Pair ("TemplateName", String.to_query v.template_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("ResourceARN", String.to_json v.resource_a_r_n)
       ; Some ("TemplateName", String.to_json v.template_name)
       ])

let of_json j =
  { template_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TemplateName"))
  ; resource_a_r_n =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceARN"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
