open Aws.BaseTypes

type t =
  { certificate_revocation_list : String.t option
  ; status : ClientCertificateRevocationListStatus.t option
  }

let make ?certificate_revocation_list ?status () = { certificate_revocation_list; status }

let parse xml =
  Some
    { certificate_revocation_list =
        Aws.Util.option_bind (Aws.Xml.member "certificateRevocationList" xml) String.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          ClientCertificateRevocationListStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientCertificateRevocationListStatus.to_query f))
       ; Aws.Util.option_map v.certificate_revocation_list (fun f ->
             Aws.Query.Pair ("CertificateRevocationList", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             "status", ClientCertificateRevocationListStatus.to_json f)
       ; Aws.Util.option_map v.certificate_revocation_list (fun f ->
             "certificateRevocationList", String.to_json f)
       ])

let of_json j =
  { certificate_revocation_list =
      Aws.Util.option_map (Aws.Json.lookup j "certificateRevocationList") String.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        ClientCertificateRevocationListStatus.of_json
  }
