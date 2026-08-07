open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_policy_id : String.t
  ; organization_target_id : String.t option
  }

let make ?dry_run ~ipam_policy_id ?organization_target_id () =
  { dry_run; ipam_policy_id; organization_target_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_policy_id =
        Aws.Xml.required
          "IpamPolicyId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPolicyId" xml) String.parse)
    ; organization_target_id =
        Aws.Util.option_bind (Aws.Xml.member "OrganizationTargetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organization_target_id (fun f ->
             Aws.Query.Pair ("OrganizationTargetId", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamPolicyId", String.to_query v.ipam_policy_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organization_target_id (fun f ->
             "OrganizationTargetId", String.to_json f)
       ; Some ("IpamPolicyId", String.to_json v.ipam_policy_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_policy_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPolicyId"))
  ; organization_target_id =
      Aws.Util.option_map (Aws.Json.lookup j "OrganizationTargetId") String.of_json
  }
