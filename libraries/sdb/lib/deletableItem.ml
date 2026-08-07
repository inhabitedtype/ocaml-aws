open Aws.BaseTypes

type t =
  { name : String.t
  ; attributes : AttributeList.t
  }

let make ~name ?(attributes = []) () = { name; attributes }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "ItemName"
          (Aws.Util.option_bind (Aws.Xml.member "ItemName" xml) String.parse)
    ; attributes = Aws.Util.of_option [] (AttributeList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Attributes.member", AttributeList.to_query v.attributes))
       ; Some (Aws.Query.Pair ("ItemName", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Attributes", AttributeList.to_json v.attributes)
       ; Some ("ItemName", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ItemName"))
  ; attributes =
      AttributeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
  }
