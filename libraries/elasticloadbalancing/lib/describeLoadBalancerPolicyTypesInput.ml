type t = { policy_type_names : PolicyTypeNames.t }

let make ?(policy_type_names = []) () = { policy_type_names }

let parse xml =
  Some
    { policy_type_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyTypeNames" xml)
             PolicyTypeNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("PolicyTypeNames.member", PolicyTypeNames.to_query v.policy_type_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyTypeNames", PolicyTypeNames.to_json v.policy_type_names) ])

let of_json j =
  { policy_type_names =
      PolicyTypeNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyTypeNames"))
  }
