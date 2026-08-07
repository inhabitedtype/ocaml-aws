open Aws.BaseTypes

type t =
  { resource_type : TagResourceType.t
  ; resource_id : String.t
  ; add_tags : TagList.t
  ; remove_tag_keys : TagKeyList.t
  }

let make ~resource_type ~resource_id ?(add_tags = []) ?(remove_tag_keys = []) () =
  { resource_type; resource_id; add_tags; remove_tag_keys }

let parse xml =
  Some
    { resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) TagResourceType.parse)
    ; resource_id =
        Aws.Xml.required
          "ResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
    ; add_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AddTags" xml) TagList.parse)
    ; remove_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "RemoveTagKeys" xml) TagKeyList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("RemoveTagKeys.member", TagKeyList.to_query v.remove_tag_keys))
       ; Some (Aws.Query.Pair ("AddTags.member", TagList.to_query v.add_tags))
       ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
       ; Some (Aws.Query.Pair ("ResourceType", TagResourceType.to_query v.resource_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("RemoveTagKeys", TagKeyList.to_json v.remove_tag_keys)
       ; Some ("AddTags", TagList.to_json v.add_tags)
       ; Some ("ResourceId", String.to_json v.resource_id)
       ; Some ("ResourceType", TagResourceType.to_json v.resource_type)
       ])

let of_json j =
  { resource_type =
      TagResourceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; resource_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
  ; add_tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AddTags"))
  ; remove_tag_keys =
      TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveTagKeys"))
  }
