open Aws.BaseTypes

type t =
  { credentials : Credentials.t option
  ; federated_user : FederatedUser.t option
  ; packed_policy_size : Integer.t option
  }

let make ?credentials ?federated_user ?packed_policy_size () =
  { credentials; federated_user; packed_policy_size }

let parse xml =
  Some
    { credentials =
        Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
    ; federated_user =
        Aws.Util.option_bind (Aws.Xml.member "FederatedUser" xml) FederatedUser.parse
    ; packed_policy_size =
        Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.packed_policy_size (fun f ->
             Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
       ; Aws.Util.option_map v.federated_user (fun f ->
             Aws.Query.Pair ("FederatedUser", FederatedUser.to_query f))
       ; Aws.Util.option_map v.credentials (fun f ->
             Aws.Query.Pair ("Credentials", Credentials.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.packed_policy_size (fun f ->
             "PackedPolicySize", Integer.to_json f)
       ; Aws.Util.option_map v.federated_user (fun f ->
             "FederatedUser", FederatedUser.to_json f)
       ; Aws.Util.option_map v.credentials (fun f -> "Credentials", Credentials.to_json f)
       ])

let of_json j =
  { credentials =
      Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
  ; federated_user =
      Aws.Util.option_map (Aws.Json.lookup j "FederatedUser") FederatedUser.of_json
  ; packed_policy_size =
      Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
  }
