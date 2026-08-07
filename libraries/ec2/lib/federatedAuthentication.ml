open Aws.BaseTypes

type t =
  { saml_provider_arn : String.t option
  ; self_service_saml_provider_arn : String.t option
  }

let make ?saml_provider_arn ?self_service_saml_provider_arn () =
  { saml_provider_arn; self_service_saml_provider_arn }

let parse xml =
  Some
    { saml_provider_arn =
        Aws.Util.option_bind (Aws.Xml.member "samlProviderArn" xml) String.parse
    ; self_service_saml_provider_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "selfServiceSamlProviderArn" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.self_service_saml_provider_arn (fun f ->
             Aws.Query.Pair ("SelfServiceSamlProviderArn", String.to_query f))
       ; Aws.Util.option_map v.saml_provider_arn (fun f ->
             Aws.Query.Pair ("SamlProviderArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.self_service_saml_provider_arn (fun f ->
             "selfServiceSamlProviderArn", String.to_json f)
       ; Aws.Util.option_map v.saml_provider_arn (fun f ->
             "samlProviderArn", String.to_json f)
       ])

let of_json j =
  { saml_provider_arn =
      Aws.Util.option_map (Aws.Json.lookup j "samlProviderArn") String.of_json
  ; self_service_saml_provider_arn =
      Aws.Util.option_map (Aws.Json.lookup j "selfServiceSamlProviderArn") String.of_json
  }
