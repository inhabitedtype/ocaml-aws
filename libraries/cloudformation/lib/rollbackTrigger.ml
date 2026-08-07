open Aws.BaseTypes

type t =
  { arn : String.t
  ; type_ : String.t
  }

let make ~arn ~type_ () = { arn; type_ }

let parse xml =
  Some
    { arn =
        Aws.Xml.required
          "Arn"
          (Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse)
    ; type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Type", String.to_query v.type_))
       ; Some (Aws.Query.Pair ("Arn", String.to_query v.arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Type", String.to_json v.type_); Some ("Arn", String.to_json v.arn) ])

let of_json j =
  { arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Arn"))
  ; type_ = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  }
