type t = { delegation_set : DelegationSet.t }

let make ~delegation_set () = { delegation_set }

let parse xml =
  Some
    { delegation_set =
        Aws.Xml.required
          "DelegationSet"
          (Aws.Util.option_bind (Aws.Xml.member "DelegationSet" xml) DelegationSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DelegationSet", DelegationSet.to_json v.delegation_set) ])

let of_json j =
  { delegation_set =
      DelegationSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSet"))
  }
