open Aws.BaseTypes

type t =
  { certificate_identifier : String.t option
  ; certificate_type : String.t option
  ; thumbprint : String.t option
  ; valid_from : DateTime.t option
  ; valid_till : DateTime.t option
  ; certificate_arn : String.t option
  ; customer_override : Boolean.t option
  ; customer_override_valid_till : DateTime.t option
  }

let make
    ?certificate_identifier
    ?certificate_type
    ?thumbprint
    ?valid_from
    ?valid_till
    ?certificate_arn
    ?customer_override
    ?customer_override_valid_till
    () =
  { certificate_identifier
  ; certificate_type
  ; thumbprint
  ; valid_from
  ; valid_till
  ; certificate_arn
  ; customer_override
  ; customer_override_valid_till
  }

let parse xml =
  Some
    { certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CertificateIdentifier" xml) String.parse
    ; certificate_type =
        Aws.Util.option_bind (Aws.Xml.member "CertificateType" xml) String.parse
    ; thumbprint = Aws.Util.option_bind (Aws.Xml.member "Thumbprint" xml) String.parse
    ; valid_from = Aws.Util.option_bind (Aws.Xml.member "ValidFrom" xml) DateTime.parse
    ; valid_till = Aws.Util.option_bind (Aws.Xml.member "ValidTill" xml) DateTime.parse
    ; certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "CertificateArn" xml) String.parse
    ; customer_override =
        Aws.Util.option_bind (Aws.Xml.member "CustomerOverride" xml) Boolean.parse
    ; customer_override_valid_till =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomerOverrideValidTill" xml)
          DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customer_override_valid_till (fun f ->
             Aws.Query.Pair ("CustomerOverrideValidTill", DateTime.to_query f))
       ; Aws.Util.option_map v.customer_override (fun f ->
             Aws.Query.Pair ("CustomerOverride", Boolean.to_query f))
       ; Aws.Util.option_map v.certificate_arn (fun f ->
             Aws.Query.Pair ("CertificateArn", String.to_query f))
       ; Aws.Util.option_map v.valid_till (fun f ->
             Aws.Query.Pair ("ValidTill", DateTime.to_query f))
       ; Aws.Util.option_map v.valid_from (fun f ->
             Aws.Query.Pair ("ValidFrom", DateTime.to_query f))
       ; Aws.Util.option_map v.thumbprint (fun f ->
             Aws.Query.Pair ("Thumbprint", String.to_query f))
       ; Aws.Util.option_map v.certificate_type (fun f ->
             Aws.Query.Pair ("CertificateType", String.to_query f))
       ; Aws.Util.option_map v.certificate_identifier (fun f ->
             Aws.Query.Pair ("CertificateIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.customer_override_valid_till (fun f ->
             "CustomerOverrideValidTill", DateTime.to_json f)
       ; Aws.Util.option_map v.customer_override (fun f ->
             "CustomerOverride", Boolean.to_json f)
       ; Aws.Util.option_map v.certificate_arn (fun f ->
             "CertificateArn", String.to_json f)
       ; Aws.Util.option_map v.valid_till (fun f -> "ValidTill", DateTime.to_json f)
       ; Aws.Util.option_map v.valid_from (fun f -> "ValidFrom", DateTime.to_json f)
       ; Aws.Util.option_map v.thumbprint (fun f -> "Thumbprint", String.to_json f)
       ; Aws.Util.option_map v.certificate_type (fun f ->
             "CertificateType", String.to_json f)
       ; Aws.Util.option_map v.certificate_identifier (fun f ->
             "CertificateIdentifier", String.to_json f)
       ])

let of_json j =
  { certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CertificateIdentifier") String.of_json
  ; certificate_type =
      Aws.Util.option_map (Aws.Json.lookup j "CertificateType") String.of_json
  ; thumbprint = Aws.Util.option_map (Aws.Json.lookup j "Thumbprint") String.of_json
  ; valid_from = Aws.Util.option_map (Aws.Json.lookup j "ValidFrom") DateTime.of_json
  ; valid_till = Aws.Util.option_map (Aws.Json.lookup j "ValidTill") DateTime.of_json
  ; certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "CertificateArn") String.of_json
  ; customer_override =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerOverride") Boolean.of_json
  ; customer_override_valid_till =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerOverrideValidTill") DateTime.of_json
  }
