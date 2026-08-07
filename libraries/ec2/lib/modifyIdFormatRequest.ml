open Aws.BaseTypes

type t =
  { resource : String.t
  ; use_long_ids : Boolean.t
  }

let make ~resource ~use_long_ids () = { resource; use_long_ids }

let parse xml =
  Some
    { resource =
        Aws.Xml.required
          "Resource"
          (Aws.Util.option_bind (Aws.Xml.member "Resource" xml) String.parse)
    ; use_long_ids =
        Aws.Xml.required
          "UseLongIds"
          (Aws.Util.option_bind (Aws.Xml.member "UseLongIds" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("UseLongIds", Boolean.to_query v.use_long_ids))
       ; Some (Aws.Query.Pair ("Resource", String.to_query v.resource))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("UseLongIds", Boolean.to_json v.use_long_ids)
       ; Some ("Resource", String.to_json v.resource)
       ])

let of_json j =
  { resource = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Resource"))
  ; use_long_ids =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UseLongIds"))
  }
