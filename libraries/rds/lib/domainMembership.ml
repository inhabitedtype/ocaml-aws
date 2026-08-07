open Aws.BaseTypes

type t =
  { domain : String.t option
  ; status : String.t option
  ; f_q_d_n : String.t option
  ; i_a_m_role_name : String.t option
  ; o_u : String.t option
  ; auth_secret_arn : String.t option
  ; dns_ips : StringList.t
  }

let make
    ?domain
    ?status
    ?f_q_d_n
    ?i_a_m_role_name
    ?o_u
    ?auth_secret_arn
    ?(dns_ips = [])
    () =
  { domain; status; f_q_d_n; i_a_m_role_name; o_u; auth_secret_arn; dns_ips }

let parse xml =
  Some
    { domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; f_q_d_n = Aws.Util.option_bind (Aws.Xml.member "FQDN" xml) String.parse
    ; i_a_m_role_name =
        Aws.Util.option_bind (Aws.Xml.member "IAMRoleName" xml) String.parse
    ; o_u = Aws.Util.option_bind (Aws.Xml.member "OU" xml) String.parse
    ; auth_secret_arn =
        Aws.Util.option_bind (Aws.Xml.member "AuthSecretArn" xml) String.parse
    ; dns_ips =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DnsIps" xml) StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DnsIps.member", StringList.to_query v.dns_ips))
       ; Aws.Util.option_map v.auth_secret_arn (fun f ->
             Aws.Query.Pair ("AuthSecretArn", String.to_query f))
       ; Aws.Util.option_map v.o_u (fun f -> Aws.Query.Pair ("OU", String.to_query f))
       ; Aws.Util.option_map v.i_a_m_role_name (fun f ->
             Aws.Query.Pair ("IAMRoleName", String.to_query f))
       ; Aws.Util.option_map v.f_q_d_n (fun f ->
             Aws.Query.Pair ("FQDN", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DnsIps", StringList.to_json v.dns_ips)
       ; Aws.Util.option_map v.auth_secret_arn (fun f ->
             "AuthSecretArn", String.to_json f)
       ; Aws.Util.option_map v.o_u (fun f -> "OU", String.to_json f)
       ; Aws.Util.option_map v.i_a_m_role_name (fun f -> "IAMRoleName", String.to_json f)
       ; Aws.Util.option_map v.f_q_d_n (fun f -> "FQDN", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", String.to_json f)
       ])

let of_json j =
  { domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; f_q_d_n = Aws.Util.option_map (Aws.Json.lookup j "FQDN") String.of_json
  ; i_a_m_role_name = Aws.Util.option_map (Aws.Json.lookup j "IAMRoleName") String.of_json
  ; o_u = Aws.Util.option_map (Aws.Json.lookup j "OU") String.of_json
  ; auth_secret_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AuthSecretArn") String.of_json
  ; dns_ips = StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DnsIps"))
  }
