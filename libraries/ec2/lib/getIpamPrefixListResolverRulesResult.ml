open Aws.BaseTypes

type t =
  { rules : IpamPrefixListResolverRuleSet.t
  ; next_token : String.t option
  }

let make ?(rules = []) ?next_token () = { rules; next_token }

let parse xml =
  Some
    { rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ruleSet" xml)
             IpamPrefixListResolverRuleSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("RuleSet", IpamPrefixListResolverRuleSet.to_query v.rules))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("ruleSet", IpamPrefixListResolverRuleSet.to_json v.rules)
       ])

let of_json j =
  { rules =
      IpamPrefixListResolverRuleSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ruleSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
