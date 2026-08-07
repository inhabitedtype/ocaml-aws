type t = { errors : ErrorSet.t }

let make ?(errors = []) () = { errors }

let parse xml =
  Some
    { errors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "errorSet" xml) ErrorSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ErrorSet", ErrorSet.to_query v.errors)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("errorSet", ErrorSet.to_json v.errors) ])

let of_json j =
  { errors = ErrorSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "errorSet")) }
