open Aws.BaseTypes

type t =
  { key_pair_id : String.t option
  ; key_type : KeyType.t option
  ; tags : TagList.t
  ; public_key : String.t option
  ; create_time : DateTime.t option
  ; key_name : String.t option
  ; key_fingerprint : String.t option
  }

let make
    ?key_pair_id
    ?key_type
    ?(tags = [])
    ?public_key
    ?create_time
    ?key_name
    ?key_fingerprint
    () =
  { key_pair_id; key_type; tags; public_key; create_time; key_name; key_fingerprint }

let parse xml =
  Some
    { key_pair_id = Aws.Util.option_bind (Aws.Xml.member "keyPairId" xml) String.parse
    ; key_type = Aws.Util.option_bind (Aws.Xml.member "keyType" xml) KeyType.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; public_key = Aws.Util.option_bind (Aws.Xml.member "publicKey" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "keyName" xml) String.parse
    ; key_fingerprint =
        Aws.Util.option_bind (Aws.Xml.member "keyFingerprint" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_fingerprint (fun f ->
             Aws.Query.Pair ("KeyFingerprint", String.to_query f))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.public_key (fun f ->
             Aws.Query.Pair ("PublicKey", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.key_type (fun f ->
             Aws.Query.Pair ("KeyType", KeyType.to_query f))
       ; Aws.Util.option_map v.key_pair_id (fun f ->
             Aws.Query.Pair ("KeyPairId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_fingerprint (fun f ->
             "keyFingerprint", String.to_json f)
       ; Aws.Util.option_map v.key_name (fun f -> "keyName", String.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.public_key (fun f -> "publicKey", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.key_type (fun f -> "keyType", KeyType.to_json f)
       ; Aws.Util.option_map v.key_pair_id (fun f -> "keyPairId", String.to_json f)
       ])

let of_json j =
  { key_pair_id = Aws.Util.option_map (Aws.Json.lookup j "keyPairId") String.of_json
  ; key_type = Aws.Util.option_map (Aws.Json.lookup j "keyType") KeyType.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; public_key = Aws.Util.option_map (Aws.Json.lookup j "publicKey") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "keyName") String.of_json
  ; key_fingerprint =
      Aws.Util.option_map (Aws.Json.lookup j "keyFingerprint") String.of_json
  }
