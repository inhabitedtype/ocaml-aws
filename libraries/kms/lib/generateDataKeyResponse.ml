open Aws.BaseTypes

type t =
  { ciphertext_blob : Blob.t
  ; plaintext : Blob.t
  ; key_id : String.t
  ; ciphertext_for_recipient : Blob.t option
  ; key_material_id : String.t option
  }

let make ~ciphertext_blob ~plaintext ~key_id ?ciphertext_for_recipient ?key_material_id ()
    =
  { ciphertext_blob; plaintext; key_id; ciphertext_for_recipient; key_material_id }

let parse xml =
  Some
    { ciphertext_blob =
        Aws.Xml.required
          "CiphertextBlob"
          (Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse)
    ; plaintext =
        Aws.Xml.required
          "Plaintext"
          (Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse)
    ; key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; ciphertext_for_recipient =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextForRecipient" xml) Blob.parse
    ; key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "KeyMaterialId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             Aws.Query.Pair ("KeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             Aws.Query.Pair ("CiphertextForRecipient", Blob.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ; Some (Aws.Query.Pair ("Plaintext", Blob.to_query v.plaintext))
       ; Some (Aws.Query.Pair ("CiphertextBlob", Blob.to_query v.ciphertext_blob))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             "KeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             "CiphertextForRecipient", Blob.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ; Some ("Plaintext", Blob.to_json v.plaintext)
       ; Some ("CiphertextBlob", Blob.to_json v.ciphertext_blob)
       ])

let of_json j =
  { ciphertext_blob =
      Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CiphertextBlob"))
  ; plaintext = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Plaintext"))
  ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; ciphertext_for_recipient =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextForRecipient") Blob.of_json
  ; key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialId") String.of_json
  }
