open Aws.BaseTypes

type t =
  { name : String.t
  ; type_ : String.t
  }

let make ~name ~type_ () = { name; type_ }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Type", String.to_query v.type_))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Type", String.to_json v.type_); Some ("Name", String.to_json v.name) ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; type_ = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  }
