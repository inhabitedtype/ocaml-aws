open Aws.BaseTypes

type t =
  { key_fingerprint : String.t option
  ; key_name : String.t option
  ; key_pair_id : String.t option
  ; tags : TagList.t
  }

let make ?key_fingerprint ?key_name ?key_pair_id ?(tags = []) () =
  { key_fingerprint; key_name; key_pair_id; tags }

let parse xml =
  Some
    { key_fingerprint =
        Aws.Util.option_bind (Aws.Xml.member "keyFingerprint" xml) String.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "keyName" xml) String.parse
    ; key_pair_id = Aws.Util.option_bind (Aws.Xml.member "keyPairId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.key_pair_id (fun f ->
             Aws.Query.Pair ("KeyPairId", String.to_query f))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.key_fingerprint (fun f ->
             Aws.Query.Pair ("KeyFingerprint", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.key_pair_id (fun f -> "keyPairId", String.to_json f)
       ; Aws.Util.option_map v.key_name (fun f -> "keyName", String.to_json f)
       ; Aws.Util.option_map v.key_fingerprint (fun f ->
             "keyFingerprint", String.to_json f)
       ])

let of_json j =
  { key_fingerprint =
      Aws.Util.option_map (Aws.Json.lookup j "keyFingerprint") String.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "keyName") String.of_json
  ; key_pair_id = Aws.Util.option_map (Aws.Json.lookup j "keyPairId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
