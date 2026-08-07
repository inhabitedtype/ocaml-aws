type t = { type_ : AccountLimitType.t }

let make ~type_ () = { type_ }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) AccountLimitType.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Type", AccountLimitType.to_query v.type_)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Type", AccountLimitType.to_json v.type_) ])

let of_json j =
  { type_ = AccountLimitType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type")) }
