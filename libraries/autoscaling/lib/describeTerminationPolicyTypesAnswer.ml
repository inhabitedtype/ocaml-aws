type t = { termination_policy_types : TerminationPolicies.t }

let make ?(termination_policy_types = []) () = { termination_policy_types }

let parse xml =
  Some
    { termination_policy_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TerminationPolicyTypes" xml)
             TerminationPolicies.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TerminationPolicyTypes.member"
              , TerminationPolicies.to_query v.termination_policy_types ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "TerminationPolicyTypes"
           , TerminationPolicies.to_json v.termination_policy_types )
       ])

let of_json j =
  { termination_policy_types =
      TerminationPolicies.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TerminationPolicyTypes"))
  }
