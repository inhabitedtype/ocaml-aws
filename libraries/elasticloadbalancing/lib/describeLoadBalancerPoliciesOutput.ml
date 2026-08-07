type t = { policy_descriptions : PolicyDescriptions.t }

let make ?(policy_descriptions = []) () = { policy_descriptions }

let parse xml =
  Some
    { policy_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyDescriptions" xml)
             PolicyDescriptions.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PolicyDescriptions.member"
              , PolicyDescriptions.to_query v.policy_descriptions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyDescriptions", PolicyDescriptions.to_json v.policy_descriptions) ])

let of_json j =
  { policy_descriptions =
      PolicyDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyDescriptions"))
  }
