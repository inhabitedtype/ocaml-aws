type t = { managed_rules : ManagedRules.t }

let make ~managed_rules () = { managed_rules }

let parse xml =
  Some
    { managed_rules =
        Aws.Xml.required
          "ManagedRules"
          (Aws.Util.option_bind (Aws.Xml.member "ManagedRules" xml) ManagedRules.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ManagedRules.member", ManagedRules.to_query v.managed_rules))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ManagedRules", ManagedRules.to_json v.managed_rules) ])

let of_json j =
  { managed_rules =
      ManagedRules.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ManagedRules"))
  }
