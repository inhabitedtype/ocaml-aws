open Aws.BaseTypes

type t =
  { credentials : Credentials.t option
  ; packed_policy_size : Integer.t option
  ; assumed_principal : String.t option
  }

let make ?credentials ?packed_policy_size ?assumed_principal () =
  { credentials; packed_policy_size; assumed_principal }

let parse xml =
  Some
    { credentials =
        Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
    ; packed_policy_size =
        Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
    ; assumed_principal =
        Aws.Util.option_bind (Aws.Xml.member "AssumedPrincipal" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.assumed_principal (fun f ->
             Aws.Query.Pair ("AssumedPrincipal", String.to_query f))
       ; Aws.Util.option_map v.packed_policy_size (fun f ->
             Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
       ; Aws.Util.option_map v.credentials (fun f ->
             Aws.Query.Pair ("Credentials", Credentials.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.assumed_principal (fun f ->
             "AssumedPrincipal", String.to_json f)
       ; Aws.Util.option_map v.packed_policy_size (fun f ->
             "PackedPolicySize", Integer.to_json f)
       ; Aws.Util.option_map v.credentials (fun f -> "Credentials", Credentials.to_json f)
       ])

let of_json j =
  { credentials =
      Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
  ; packed_policy_size =
      Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
  ; assumed_principal =
      Aws.Util.option_map (Aws.Json.lookup j "AssumedPrincipal") String.of_json
  }
