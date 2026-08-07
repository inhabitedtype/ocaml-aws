open Aws.BaseTypes

type t =
  { key_id : String.t
  ; import_token : Blob.t
  ; encrypted_key_material : Blob.t
  ; valid_to : DateTime.t option
  ; expiration_model : ExpirationModelType.t option
  ; import_type : ImportType.t option
  ; key_material_description : String.t option
  ; key_material_id : String.t option
  }

let make
    ~key_id
    ~import_token
    ~encrypted_key_material
    ?valid_to
    ?expiration_model
    ?import_type
    ?key_material_description
    ?key_material_id
    () =
  { key_id
  ; import_token
  ; encrypted_key_material
  ; valid_to
  ; expiration_model
  ; import_type
  ; key_material_description
  ; key_material_id
  }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; import_token =
        Aws.Xml.required
          "ImportToken"
          (Aws.Util.option_bind (Aws.Xml.member "ImportToken" xml) Blob.parse)
    ; encrypted_key_material =
        Aws.Xml.required
          "EncryptedKeyMaterial"
          (Aws.Util.option_bind (Aws.Xml.member "EncryptedKeyMaterial" xml) Blob.parse)
    ; valid_to = Aws.Util.option_bind (Aws.Xml.member "ValidTo" xml) DateTime.parse
    ; expiration_model =
        Aws.Util.option_bind
          (Aws.Xml.member "ExpirationModel" xml)
          ExpirationModelType.parse
    ; import_type =
        Aws.Util.option_bind (Aws.Xml.member "ImportType" xml) ImportType.parse
    ; key_material_description =
        Aws.Util.option_bind (Aws.Xml.member "KeyMaterialDescription" xml) String.parse
    ; key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "KeyMaterialId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             Aws.Query.Pair ("KeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.key_material_description (fun f ->
             Aws.Query.Pair ("KeyMaterialDescription", String.to_query f))
       ; Aws.Util.option_map v.import_type (fun f ->
             Aws.Query.Pair ("ImportType", ImportType.to_query f))
       ; Aws.Util.option_map v.expiration_model (fun f ->
             Aws.Query.Pair ("ExpirationModel", ExpirationModelType.to_query f))
       ; Aws.Util.option_map v.valid_to (fun f ->
             Aws.Query.Pair ("ValidTo", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair ("EncryptedKeyMaterial", Blob.to_query v.encrypted_key_material))
       ; Some (Aws.Query.Pair ("ImportToken", Blob.to_query v.import_token))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             "KeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.key_material_description (fun f ->
             "KeyMaterialDescription", String.to_json f)
       ; Aws.Util.option_map v.import_type (fun f -> "ImportType", ImportType.to_json f)
       ; Aws.Util.option_map v.expiration_model (fun f ->
             "ExpirationModel", ExpirationModelType.to_json f)
       ; Aws.Util.option_map v.valid_to (fun f -> "ValidTo", DateTime.to_json f)
       ; Some ("EncryptedKeyMaterial", Blob.to_json v.encrypted_key_material)
       ; Some ("ImportToken", Blob.to_json v.import_token)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; import_token = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImportToken"))
  ; encrypted_key_material =
      Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EncryptedKeyMaterial"))
  ; valid_to = Aws.Util.option_map (Aws.Json.lookup j "ValidTo") DateTime.of_json
  ; expiration_model =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExpirationModel")
        ExpirationModelType.of_json
  ; import_type = Aws.Util.option_map (Aws.Json.lookup j "ImportType") ImportType.of_json
  ; key_material_description =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialDescription") String.of_json
  ; key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialId") String.of_json
  }
