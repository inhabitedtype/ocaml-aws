open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; key_material_id : String.t option
  ; key_material_description : String.t option
  ; import_state : ImportState.t option
  ; key_material_state : KeyMaterialState.t option
  ; expiration_model : ExpirationModelType.t option
  ; valid_to : DateTime.t option
  ; rotation_date : DateTime.t option
  ; rotation_type : RotationType.t option
  }

let make
    ?key_id
    ?key_material_id
    ?key_material_description
    ?import_state
    ?key_material_state
    ?expiration_model
    ?valid_to
    ?rotation_date
    ?rotation_type
    () =
  { key_id
  ; key_material_id
  ; key_material_description
  ; import_state
  ; key_material_state
  ; expiration_model
  ; valid_to
  ; rotation_date
  ; rotation_type
  }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "KeyMaterialId" xml) String.parse
    ; key_material_description =
        Aws.Util.option_bind (Aws.Xml.member "KeyMaterialDescription" xml) String.parse
    ; import_state =
        Aws.Util.option_bind (Aws.Xml.member "ImportState" xml) ImportState.parse
    ; key_material_state =
        Aws.Util.option_bind
          (Aws.Xml.member "KeyMaterialState" xml)
          KeyMaterialState.parse
    ; expiration_model =
        Aws.Util.option_bind
          (Aws.Xml.member "ExpirationModel" xml)
          ExpirationModelType.parse
    ; valid_to = Aws.Util.option_bind (Aws.Xml.member "ValidTo" xml) DateTime.parse
    ; rotation_date =
        Aws.Util.option_bind (Aws.Xml.member "RotationDate" xml) DateTime.parse
    ; rotation_type =
        Aws.Util.option_bind (Aws.Xml.member "RotationType" xml) RotationType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rotation_type (fun f ->
             Aws.Query.Pair ("RotationType", RotationType.to_query f))
       ; Aws.Util.option_map v.rotation_date (fun f ->
             Aws.Query.Pair ("RotationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.valid_to (fun f ->
             Aws.Query.Pair ("ValidTo", DateTime.to_query f))
       ; Aws.Util.option_map v.expiration_model (fun f ->
             Aws.Query.Pair ("ExpirationModel", ExpirationModelType.to_query f))
       ; Aws.Util.option_map v.key_material_state (fun f ->
             Aws.Query.Pair ("KeyMaterialState", KeyMaterialState.to_query f))
       ; Aws.Util.option_map v.import_state (fun f ->
             Aws.Query.Pair ("ImportState", ImportState.to_query f))
       ; Aws.Util.option_map v.key_material_description (fun f ->
             Aws.Query.Pair ("KeyMaterialDescription", String.to_query f))
       ; Aws.Util.option_map v.key_material_id (fun f ->
             Aws.Query.Pair ("KeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rotation_type (fun f ->
             "RotationType", RotationType.to_json f)
       ; Aws.Util.option_map v.rotation_date (fun f -> "RotationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.valid_to (fun f -> "ValidTo", DateTime.to_json f)
       ; Aws.Util.option_map v.expiration_model (fun f ->
             "ExpirationModel", ExpirationModelType.to_json f)
       ; Aws.Util.option_map v.key_material_state (fun f ->
             "KeyMaterialState", KeyMaterialState.to_json f)
       ; Aws.Util.option_map v.import_state (fun f ->
             "ImportState", ImportState.to_json f)
       ; Aws.Util.option_map v.key_material_description (fun f ->
             "KeyMaterialDescription", String.to_json f)
       ; Aws.Util.option_map v.key_material_id (fun f ->
             "KeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialId") String.of_json
  ; key_material_description =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialDescription") String.of_json
  ; import_state =
      Aws.Util.option_map (Aws.Json.lookup j "ImportState") ImportState.of_json
  ; key_material_state =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialState") KeyMaterialState.of_json
  ; expiration_model =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExpirationModel")
        ExpirationModelType.of_json
  ; valid_to = Aws.Util.option_map (Aws.Json.lookup j "ValidTo") DateTime.of_json
  ; rotation_date =
      Aws.Util.option_map (Aws.Json.lookup j "RotationDate") DateTime.of_json
  ; rotation_type =
      Aws.Util.option_map (Aws.Json.lookup j "RotationType") RotationType.of_json
  }
