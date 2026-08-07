open Aws.BaseTypes

type t =
  { organization_targets : IpamPolicyOrganizationTargetSet.t
  ; next_token : String.t option
  }

let make ?(organization_targets = []) ?next_token () =
  { organization_targets; next_token }

let parse xml =
  Some
    { organization_targets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "organizationTargetSet" xml)
             IpamPolicyOrganizationTargetSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OrganizationTargetSet"
              , IpamPolicyOrganizationTargetSet.to_query v.organization_targets ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "organizationTargetSet"
           , IpamPolicyOrganizationTargetSet.to_json v.organization_targets )
       ])

let of_json j =
  { organization_targets =
      IpamPolicyOrganizationTargetSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "organizationTargetSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
