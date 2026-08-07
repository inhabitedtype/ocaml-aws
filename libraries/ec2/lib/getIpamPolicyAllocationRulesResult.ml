open Aws.BaseTypes

type t =
  { ipam_policy_documents : IpamPolicyDocumentSet.t
  ; next_token : String.t option
  }

let make ?(ipam_policy_documents = []) ?next_token () =
  { ipam_policy_documents; next_token }

let parse xml =
  Some
    { ipam_policy_documents =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipamPolicyDocumentSet" xml)
             IpamPolicyDocumentSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IpamPolicyDocumentSet"
              , IpamPolicyDocumentSet.to_query v.ipam_policy_documents ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("ipamPolicyDocumentSet", IpamPolicyDocumentSet.to_json v.ipam_policy_documents)
       ])

let of_json j =
  { ipam_policy_documents =
      IpamPolicyDocumentSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamPolicyDocumentSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
