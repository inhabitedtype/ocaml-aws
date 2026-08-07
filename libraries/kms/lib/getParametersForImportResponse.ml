open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; import_token : Blob.t option
  ; public_key : Blob.t option
  ; parameters_valid_to : DateTime.t option
  }

let make ?key_id ?import_token ?public_key ?parameters_valid_to () =
  { key_id; import_token; public_key; parameters_valid_to }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; import_token = Aws.Util.option_bind (Aws.Xml.member "ImportToken" xml) Blob.parse
    ; public_key = Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) Blob.parse
    ; parameters_valid_to =
        Aws.Util.option_bind (Aws.Xml.member "ParametersValidTo" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.parameters_valid_to (fun f ->
             Aws.Query.Pair ("ParametersValidTo", DateTime.to_query f))
       ; Aws.Util.option_map v.public_key (fun f ->
             Aws.Query.Pair ("PublicKey", Blob.to_query f))
       ; Aws.Util.option_map v.import_token (fun f ->
             Aws.Query.Pair ("ImportToken", Blob.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.parameters_valid_to (fun f ->
             "ParametersValidTo", DateTime.to_json f)
       ; Aws.Util.option_map v.public_key (fun f -> "PublicKey", Blob.to_json f)
       ; Aws.Util.option_map v.import_token (fun f -> "ImportToken", Blob.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; import_token = Aws.Util.option_map (Aws.Json.lookup j "ImportToken") Blob.of_json
  ; public_key = Aws.Util.option_map (Aws.Json.lookup j "PublicKey") Blob.of_json
  ; parameters_valid_to =
      Aws.Util.option_map (Aws.Json.lookup j "ParametersValidTo") DateTime.of_json
  }
