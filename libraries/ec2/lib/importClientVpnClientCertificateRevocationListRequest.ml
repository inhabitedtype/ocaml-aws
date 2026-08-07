open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; certificate_revocation_list : String.t
  ; dry_run : Boolean.t option
  }

let make ~client_vpn_endpoint_id ~certificate_revocation_list ?dry_run () =
  { client_vpn_endpoint_id; certificate_revocation_list; dry_run }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; certificate_revocation_list =
        Aws.Xml.required
          "CertificateRevocationList"
          (Aws.Util.option_bind
             (Aws.Xml.member "CertificateRevocationList" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CertificateRevocationList", String.to_query v.certificate_revocation_list))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("CertificateRevocationList", String.to_json v.certificate_revocation_list)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; certificate_revocation_list =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CertificateRevocationList"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
