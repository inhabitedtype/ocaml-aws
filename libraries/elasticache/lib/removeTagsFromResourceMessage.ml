open Aws.BaseTypes

type t =
  { resource_name : String.t
  ; tag_keys : KeyList.t
  }

let make ~resource_name ~tag_keys () = { resource_name; tag_keys }

let parse xml =
  Some
    { resource_name =
        Aws.Xml.required
          "ResourceName"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceName" xml) String.parse)
    ; tag_keys =
        Aws.Xml.required
          "TagKeys"
          (Aws.Util.option_bind (Aws.Xml.member "TagKeys" xml) KeyList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagKeys.member", KeyList.to_query v.tag_keys))
       ; Some (Aws.Query.Pair ("ResourceName", String.to_query v.resource_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagKeys", KeyList.to_json v.tag_keys)
       ; Some ("ResourceName", String.to_json v.resource_name)
       ])

let of_json j =
  { resource_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceName"))
  ; tag_keys = KeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKeys"))
  }
