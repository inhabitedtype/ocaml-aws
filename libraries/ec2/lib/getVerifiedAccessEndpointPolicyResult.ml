open Aws.BaseTypes

type t =
  { policy_enabled : Boolean.t option
  ; policy_document : String.t option
  }

let make ?policy_enabled ?policy_document () = { policy_enabled; policy_document }

let parse xml =
  Some
    { policy_enabled =
        Aws.Util.option_bind (Aws.Xml.member "policyEnabled" xml) Boolean.parse
    ; policy_document =
        Aws.Util.option_bind (Aws.Xml.member "policyDocument" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_document (fun f ->
             Aws.Query.Pair ("PolicyDocument", String.to_query f))
       ; Aws.Util.option_map v.policy_enabled (fun f ->
             Aws.Query.Pair ("PolicyEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.policy_document (fun f ->
             "policyDocument", String.to_json f)
       ; Aws.Util.option_map v.policy_enabled (fun f ->
             "policyEnabled", Boolean.to_json f)
       ])

let of_json j =
  { policy_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "policyEnabled") Boolean.of_json
  ; policy_document =
      Aws.Util.option_map (Aws.Json.lookup j "policyDocument") String.of_json
  }
