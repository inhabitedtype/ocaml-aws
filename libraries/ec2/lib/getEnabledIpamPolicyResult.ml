open Aws.BaseTypes

type t =
  { ipam_policy_enabled : Boolean.t option
  ; ipam_policy_id : String.t option
  ; managed_by : IpamPolicyManagedBy.t option
  }

let make ?ipam_policy_enabled ?ipam_policy_id ?managed_by () =
  { ipam_policy_enabled; ipam_policy_id; managed_by }

let parse xml =
  Some
    { ipam_policy_enabled =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicyEnabled" xml) Boolean.parse
    ; ipam_policy_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicyId" xml) String.parse
    ; managed_by =
        Aws.Util.option_bind (Aws.Xml.member "managedBy" xml) IpamPolicyManagedBy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f ->
             Aws.Query.Pair ("ManagedBy", IpamPolicyManagedBy.to_query f))
       ; Aws.Util.option_map v.ipam_policy_id (fun f ->
             Aws.Query.Pair ("IpamPolicyId", String.to_query f))
       ; Aws.Util.option_map v.ipam_policy_enabled (fun f ->
             Aws.Query.Pair ("IpamPolicyEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f ->
             "managedBy", IpamPolicyManagedBy.to_json f)
       ; Aws.Util.option_map v.ipam_policy_id (fun f -> "ipamPolicyId", String.to_json f)
       ; Aws.Util.option_map v.ipam_policy_enabled (fun f ->
             "ipamPolicyEnabled", Boolean.to_json f)
       ])

let of_json j =
  { ipam_policy_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPolicyEnabled") Boolean.of_json
  ; ipam_policy_id = Aws.Util.option_map (Aws.Json.lookup j "ipamPolicyId") String.of_json
  ; managed_by =
      Aws.Util.option_map (Aws.Json.lookup j "managedBy") IpamPolicyManagedBy.of_json
  }
