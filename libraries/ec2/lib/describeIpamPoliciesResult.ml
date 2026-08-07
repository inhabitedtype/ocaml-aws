open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; ipam_policies : IpamPolicySet.t
  }

let make ?next_token ?(ipam_policies = []) () = { next_token; ipam_policies }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; ipam_policies =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ipamPolicySet" xml) IpamPolicySet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("IpamPolicySet", IpamPolicySet.to_query v.ipam_policies))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ipamPolicySet", IpamPolicySet.to_json v.ipam_policies)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; ipam_policies =
      IpamPolicySet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamPolicySet"))
  }
