open Aws.BaseTypes

type t =
  { managed_rules : ManagedRuleDescriptions.t
  ; next_token : String.t option
  }

let make ?(managed_rules = []) ?next_token () = { managed_rules; next_token }

let parse xml =
  Some
    { managed_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ManagedRules" xml)
             ManagedRuleDescriptions.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ManagedRules.member", ManagedRuleDescriptions.to_query v.managed_rules))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ManagedRules", ManagedRuleDescriptions.to_json v.managed_rules)
       ])

let of_json j =
  { managed_rules =
      ManagedRuleDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ManagedRules"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
