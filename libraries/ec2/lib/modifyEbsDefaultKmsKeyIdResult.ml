open Aws.BaseTypes

type t = { kms_key_id : String.t option }

let make ?kms_key_id () = { kms_key_id }

let parse xml =
  Some { kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f) ])

let of_json j =
  { kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json }
