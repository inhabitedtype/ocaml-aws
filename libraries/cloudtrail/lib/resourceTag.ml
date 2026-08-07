open Aws.BaseTypes

type t =
  { resource_id : String.t option
  ; tags_list : TagsList.t
  }

let make ?resource_id ?(tags_list = []) () = { resource_id; tags_list }

let parse xml =
  Some
    { resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
    ; tags_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagsList" xml) TagsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagsList.member", TagsList.to_query v.tags_list))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagsList", TagsList.to_json v.tags_list)
       ; Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
       ])

let of_json j =
  { resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
  ; tags_list = TagsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagsList"))
  }
