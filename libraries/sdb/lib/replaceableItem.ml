open Aws.BaseTypes

type t =
  { name : String.t
  ; attributes : ReplaceableAttributeList.t
  }

let make ~name ~attributes () = { name; attributes }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "ItemName"
          (Aws.Util.option_bind (Aws.Xml.member "ItemName" xml) String.parse)
    ; attributes = Aws.Xml.required "Attributes" (ReplaceableAttributeList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Attributes.member", ReplaceableAttributeList.to_query v.attributes))
       ; Some (Aws.Query.Pair ("ItemName", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Attributes", ReplaceableAttributeList.to_json v.attributes)
       ; Some ("ItemName", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
  ; attributes =
      ReplaceableAttributeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
  }
