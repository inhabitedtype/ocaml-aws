open Aws.BaseTypes

type t =
  { secret_arn : String.t option
  ; secret_status : String.t option
  ; kms_key_id : String.t option
  }

let make ?secret_arn ?secret_status ?kms_key_id () =
  { secret_arn; secret_status; kms_key_id }

let parse xml =
  Some
    { secret_arn = Aws.Util.option_bind (Aws.Xml.member "SecretArn" xml) String.parse
    ; secret_status =
        Aws.Util.option_bind (Aws.Xml.member "SecretStatus" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.secret_status (fun f ->
             Aws.Query.Pair ("SecretStatus", String.to_query f))
       ; Aws.Util.option_map v.secret_arn (fun f ->
             Aws.Query.Pair ("SecretArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.secret_status (fun f -> "SecretStatus", String.to_json f)
       ; Aws.Util.option_map v.secret_arn (fun f -> "SecretArn", String.to_json f)
       ])

let of_json j =
  { secret_arn = Aws.Util.option_map (Aws.Json.lookup j "SecretArn") String.of_json
  ; secret_status = Aws.Util.option_map (Aws.Json.lookup j "SecretStatus") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  }
