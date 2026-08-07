open Aws.BaseTypes

type t =
  { key_encryption_algorithm : KeyEncryptionMechanism.t option
  ; attestation_document : Blob.t option
  }

let make ?key_encryption_algorithm ?attestation_document () =
  { key_encryption_algorithm; attestation_document }

let parse xml =
  Some
    { key_encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "KeyEncryptionAlgorithm" xml)
          KeyEncryptionMechanism.parse
    ; attestation_document =
        Aws.Util.option_bind (Aws.Xml.member "AttestationDocument" xml) Blob.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attestation_document (fun f ->
             Aws.Query.Pair ("AttestationDocument", Blob.to_query f))
       ; Aws.Util.option_map v.key_encryption_algorithm (fun f ->
             Aws.Query.Pair ("KeyEncryptionAlgorithm", KeyEncryptionMechanism.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attestation_document (fun f ->
             "AttestationDocument", Blob.to_json f)
       ; Aws.Util.option_map v.key_encryption_algorithm (fun f ->
             "KeyEncryptionAlgorithm", KeyEncryptionMechanism.to_json f)
       ])

let of_json j =
  { key_encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "KeyEncryptionAlgorithm")
        KeyEncryptionMechanism.of_json
  ; attestation_document =
      Aws.Util.option_map (Aws.Json.lookup j "AttestationDocument") Blob.of_json
  }
