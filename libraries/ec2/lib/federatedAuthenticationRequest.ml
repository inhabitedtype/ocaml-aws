open Aws.BaseTypes

type t =
  { s_a_m_l_provider_arn : String.t option
  ; self_service_s_a_m_l_provider_arn : String.t option
  }

let make ?s_a_m_l_provider_arn ?self_service_s_a_m_l_provider_arn () =
  { s_a_m_l_provider_arn; self_service_s_a_m_l_provider_arn }

let parse xml =
  Some
    { s_a_m_l_provider_arn =
        Aws.Util.option_bind (Aws.Xml.member "SAMLProviderArn" xml) String.parse
    ; self_service_s_a_m_l_provider_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "SelfServiceSAMLProviderArn" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.self_service_s_a_m_l_provider_arn (fun f ->
             Aws.Query.Pair ("SelfServiceSAMLProviderArn", String.to_query f))
       ; Aws.Util.option_map v.s_a_m_l_provider_arn (fun f ->
             Aws.Query.Pair ("SAMLProviderArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.self_service_s_a_m_l_provider_arn (fun f ->
             "SelfServiceSAMLProviderArn", String.to_json f)
       ; Aws.Util.option_map v.s_a_m_l_provider_arn (fun f ->
             "SAMLProviderArn", String.to_json f)
       ])

let of_json j =
  { s_a_m_l_provider_arn =
      Aws.Util.option_map (Aws.Json.lookup j "SAMLProviderArn") String.of_json
  ; self_service_s_a_m_l_provider_arn =
      Aws.Util.option_map (Aws.Json.lookup j "SelfServiceSAMLProviderArn") String.of_json
  }
