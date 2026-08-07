open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_prefix_list_resolver_id : String.t
  ; description : String.t option
  ; rules : IpamPrefixListResolverRuleRequestSet.t
  }

let make ?dry_run ~ipam_prefix_list_resolver_id ?description ?(rules = []) () =
  { dry_run; ipam_prefix_list_resolver_id; description; rules }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_prefix_list_resolver_id =
        Aws.Xml.required
          "IpamPrefixListResolverId"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamPrefixListResolverId" xml)
             String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Rule" xml)
             IpamPrefixListResolverRuleRequestSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Rule", IpamPrefixListResolverRuleRequestSet.to_query v.rules))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("IpamPrefixListResolverId", String.to_query v.ipam_prefix_list_resolver_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Rule", IpamPrefixListResolverRuleRequestSet.to_json v.rules)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("IpamPrefixListResolverId", String.to_json v.ipam_prefix_list_resolver_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_prefix_list_resolver_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPrefixListResolverId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; rules =
      IpamPrefixListResolverRuleRequestSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule"))
  }
