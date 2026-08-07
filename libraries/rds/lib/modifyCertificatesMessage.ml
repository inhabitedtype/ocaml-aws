open Aws.BaseTypes

type t =
  { certificate_identifier : String.t option
  ; remove_customer_override : Boolean.t option
  }

let make ?certificate_identifier ?remove_customer_override () =
  { certificate_identifier; remove_customer_override }

let parse xml =
  Some
    { certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CertificateIdentifier" xml) String.parse
    ; remove_customer_override =
        Aws.Util.option_bind (Aws.Xml.member "RemoveCustomerOverride" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remove_customer_override (fun f ->
             Aws.Query.Pair ("RemoveCustomerOverride", Boolean.to_query f))
       ; Aws.Util.option_map v.certificate_identifier (fun f ->
             Aws.Query.Pair ("CertificateIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remove_customer_override (fun f ->
             "RemoveCustomerOverride", Boolean.to_json f)
       ; Aws.Util.option_map v.certificate_identifier (fun f ->
             "CertificateIdentifier", String.to_json f)
       ])

let of_json j =
  { certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CertificateIdentifier") String.of_json
  ; remove_customer_override =
      Aws.Util.option_map (Aws.Json.lookup j "RemoveCustomerOverride") Boolean.of_json
  }
