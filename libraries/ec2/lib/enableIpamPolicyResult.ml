open Aws.BaseTypes

type t = { ipam_policy_id : String.t option }

let make ?ipam_policy_id () = { ipam_policy_id }

let parse xml =
  Some
    { ipam_policy_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_policy_id (fun f ->
             Aws.Query.Pair ("IpamPolicyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_policy_id (fun f -> "ipamPolicyId", String.to_json f)
       ])

let of_json j =
  { ipam_policy_id = Aws.Util.option_map (Aws.Json.lookup j "ipamPolicyId") String.of_json
  }
