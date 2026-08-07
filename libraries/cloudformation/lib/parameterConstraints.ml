type t = { allowed_values : AllowedValues.t }

let make ?(allowed_values = []) () = { allowed_values }

let parse xml =
  Some
    { allowed_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AllowedValues" xml) AllowedValues.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AllowedValues.member", AllowedValues.to_query v.allowed_values))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AllowedValues", AllowedValues.to_json v.allowed_values) ])

let of_json j =
  { allowed_values =
      AllowedValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedValues"))
  }
