open Aws.BaseTypes

type t =
  { default_certificate_for_new_launches : String.t option
  ; certificates : CertificateList.t
  ; marker : String.t option
  }

let make ?default_certificate_for_new_launches ?(certificates = []) ?marker () =
  { default_certificate_for_new_launches; certificates; marker }

let parse xml =
  Some
    { default_certificate_for_new_launches =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultCertificateForNewLaunches" xml)
          String.parse
    ; certificates =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Certificates" xml) CertificateList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Certificates.member", CertificateList.to_query v.certificates))
       ; Aws.Util.option_map v.default_certificate_for_new_launches (fun f ->
             Aws.Query.Pair ("DefaultCertificateForNewLaunches", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("Certificates", CertificateList.to_json v.certificates)
       ; Aws.Util.option_map v.default_certificate_for_new_launches (fun f ->
             "DefaultCertificateForNewLaunches", String.to_json f)
       ])

let of_json j =
  { default_certificate_for_new_launches =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultCertificateForNewLaunches")
        String.of_json
  ; certificates =
      CertificateList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Certificates"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
