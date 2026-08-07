open Aws.BaseTypes

type t =
  { provider_arn : String.t option
  ; context_assertion : String.t option
  }

let make ?provider_arn ?context_assertion () = { provider_arn; context_assertion }

let parse xml =
  Some
    { provider_arn = Aws.Util.option_bind (Aws.Xml.member "ProviderArn" xml) String.parse
    ; context_assertion =
        Aws.Util.option_bind (Aws.Xml.member "ContextAssertion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context_assertion (fun f ->
             Aws.Query.Pair ("ContextAssertion", String.to_query f))
       ; Aws.Util.option_map v.provider_arn (fun f ->
             Aws.Query.Pair ("ProviderArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context_assertion (fun f ->
             "ContextAssertion", String.to_json f)
       ; Aws.Util.option_map v.provider_arn (fun f -> "ProviderArn", String.to_json f)
       ])

let of_json j =
  { provider_arn = Aws.Util.option_map (Aws.Json.lookup j "ProviderArn") String.of_json
  ; context_assertion =
      Aws.Util.option_map (Aws.Json.lookup j "ContextAssertion") String.of_json
  }
