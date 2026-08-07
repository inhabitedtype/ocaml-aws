open Aws.BaseTypes

type t = { client_root_certificate_chain_arn : String.t option }

let make ?client_root_certificate_chain_arn () = { client_root_certificate_chain_arn }

let parse xml =
  Some
    { client_root_certificate_chain_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "ClientRootCertificateChainArn" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_root_certificate_chain_arn (fun f ->
             Aws.Query.Pair ("ClientRootCertificateChainArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_root_certificate_chain_arn (fun f ->
             "ClientRootCertificateChainArn", String.to_json f)
       ])

let of_json j =
  { client_root_certificate_chain_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClientRootCertificateChainArn")
        String.of_json
  }
