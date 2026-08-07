open Aws.BaseTypes

type t = { client_root_certificate_chain : String.t option }

let make ?client_root_certificate_chain () = { client_root_certificate_chain }

let parse xml =
  Some
    { client_root_certificate_chain =
        Aws.Util.option_bind
          (Aws.Xml.member "clientRootCertificateChain" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_root_certificate_chain (fun f ->
             Aws.Query.Pair ("ClientRootCertificateChain", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_root_certificate_chain (fun f ->
             "clientRootCertificateChain", String.to_json f)
       ])

let of_json j =
  { client_root_certificate_chain =
      Aws.Util.option_map (Aws.Json.lookup j "clientRootCertificateChain") String.of_json
  }
