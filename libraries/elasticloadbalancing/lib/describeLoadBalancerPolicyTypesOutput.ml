type t = { policy_type_descriptions : PolicyTypeDescriptions.t }

let make ?(policy_type_descriptions = []) () = { policy_type_descriptions }

let parse xml =
  Some
    { policy_type_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyTypeDescriptions" xml)
             PolicyTypeDescriptions.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PolicyTypeDescriptions.member"
              , PolicyTypeDescriptions.to_query v.policy_type_descriptions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "PolicyTypeDescriptions"
           , PolicyTypeDescriptions.to_json v.policy_type_descriptions )
       ])

let of_json j =
  { policy_type_descriptions =
      PolicyTypeDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyTypeDescriptions"))
  }
