open Aws.BaseTypes

type t =
  { ciphertext_blob : Blob.t option
  ; key_id : String.t option
  ; key_material_id : String.t option
  }

let make ?ciphertext_blob ?key_id ?key_material_id () =
  { ciphertext_blob; key_id; key_material_id }

let parse xml =
  Some
    { ciphertext_blob =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse
    ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "KeyMaterialId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             Aws.Query.Pair ("KeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ; Aws.Util.option_map v.ciphertext_blob (fun f ->
             Aws.Query.Pair ("CiphertextBlob", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             "KeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ; Aws.Util.option_map v.ciphertext_blob (fun f -> "CiphertextBlob", Blob.to_json f)
       ])

let of_json j =
  { ciphertext_blob =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextBlob") Blob.of_json
  ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialId") String.of_json
  }
