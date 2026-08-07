open Aws.BaseTypes

type t =
  { plaintext : Blob.t option
  ; ciphertext_for_recipient : Blob.t option
  }

let make ?plaintext ?ciphertext_for_recipient () = { plaintext; ciphertext_for_recipient }

let parse xml =
  Some
    { plaintext = Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse
    ; ciphertext_for_recipient =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextForRecipient" xml) Blob.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             Aws.Query.Pair ("CiphertextForRecipient", Blob.to_query f))
       ; Aws.Util.option_map v.plaintext (fun f ->
             Aws.Query.Pair ("Plaintext", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             "CiphertextForRecipient", Blob.to_json f)
       ; Aws.Util.option_map v.plaintext (fun f -> "Plaintext", Blob.to_json f)
       ])

let of_json j =
  { plaintext = Aws.Util.option_map (Aws.Json.lookup j "Plaintext") Blob.of_json
  ; ciphertext_for_recipient =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextForRecipient") Blob.of_json
  }
