open Aws.BaseTypes

type t =
  { key_pair_id : String.t option
  ; tags : TagList.t
  ; key_name : String.t
  ; key_fingerprint : String.t
  ; key_material : String.t
  }

let make ?key_pair_id ?(tags = []) ~key_name ~key_fingerprint ~key_material () =
  { key_pair_id; tags; key_name; key_fingerprint; key_material }

let parse xml =
  Some
    { key_pair_id = Aws.Util.option_bind (Aws.Xml.member "keyPairId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; key_name =
        Aws.Xml.required
          "keyName"
          (Aws.Util.option_bind (Aws.Xml.member "keyName" xml) String.parse)
    ; key_fingerprint =
        Aws.Xml.required
          "keyFingerprint"
          (Aws.Util.option_bind (Aws.Xml.member "keyFingerprint" xml) String.parse)
    ; key_material =
        Aws.Xml.required
          "keyMaterial"
          (Aws.Util.option_bind (Aws.Xml.member "keyMaterial" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("KeyMaterial", String.to_query v.key_material))
       ; Some (Aws.Query.Pair ("KeyFingerprint", String.to_query v.key_fingerprint))
       ; Some (Aws.Query.Pair ("KeyName", String.to_query v.key_name))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.key_pair_id (fun f ->
             Aws.Query.Pair ("KeyPairId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("keyMaterial", String.to_json v.key_material)
       ; Some ("keyFingerprint", String.to_json v.key_fingerprint)
       ; Some ("keyName", String.to_json v.key_name)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.key_pair_id (fun f -> "keyPairId", String.to_json f)
       ])

let of_json j =
  { key_pair_id = Aws.Util.option_map (Aws.Json.lookup j "keyPairId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; key_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "keyName"))
  ; key_fingerprint =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "keyFingerprint"))
  ; key_material =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "keyMaterial"))
  }
