open Aws.BaseTypes

type t =
  { a_w_s_access_key_id : String.t option
  ; bucket : String.t option
  ; prefix : String.t option
  ; upload_policy : Blob.t option
  ; upload_policy_signature : String.t option
  }

let make ?a_w_s_access_key_id ?bucket ?prefix ?upload_policy ?upload_policy_signature () =
  { a_w_s_access_key_id; bucket; prefix; upload_policy; upload_policy_signature }

let parse xml =
  Some
    { a_w_s_access_key_id =
        Aws.Util.option_bind (Aws.Xml.member "AWSAccessKeyId" xml) String.parse
    ; bucket = Aws.Util.option_bind (Aws.Xml.member "bucket" xml) String.parse
    ; prefix = Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse
    ; upload_policy = Aws.Util.option_bind (Aws.Xml.member "uploadPolicy" xml) Blob.parse
    ; upload_policy_signature =
        Aws.Util.option_bind (Aws.Xml.member "uploadPolicySignature" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.upload_policy_signature (fun f ->
             Aws.Query.Pair ("UploadPolicySignature", String.to_query f))
       ; Aws.Util.option_map v.upload_policy (fun f ->
             Aws.Query.Pair ("UploadPolicy", Blob.to_query f))
       ; Aws.Util.option_map v.prefix (fun f ->
             Aws.Query.Pair ("Prefix", String.to_query f))
       ; Aws.Util.option_map v.bucket (fun f ->
             Aws.Query.Pair ("Bucket", String.to_query f))
       ; Aws.Util.option_map v.a_w_s_access_key_id (fun f ->
             Aws.Query.Pair ("AWSAccessKeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.upload_policy_signature (fun f ->
             "uploadPolicySignature", String.to_json f)
       ; Aws.Util.option_map v.upload_policy (fun f -> "uploadPolicy", Blob.to_json f)
       ; Aws.Util.option_map v.prefix (fun f -> "prefix", String.to_json f)
       ; Aws.Util.option_map v.bucket (fun f -> "bucket", String.to_json f)
       ; Aws.Util.option_map v.a_w_s_access_key_id (fun f ->
             "AWSAccessKeyId", String.to_json f)
       ])

let of_json j =
  { a_w_s_access_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "AWSAccessKeyId") String.of_json
  ; bucket = Aws.Util.option_map (Aws.Json.lookup j "bucket") String.of_json
  ; prefix = Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json
  ; upload_policy = Aws.Util.option_map (Aws.Json.lookup j "uploadPolicy") Blob.of_json
  ; upload_policy_signature =
      Aws.Util.option_map (Aws.Json.lookup j "uploadPolicySignature") String.of_json
  }
