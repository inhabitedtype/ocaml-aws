open Aws.BaseTypes

type t =
  { dataset_id : String.t
  ; arn : String.t
  ; kms_key_arn : String.t option
  }

let make ~dataset_id ~arn ?kms_key_arn () = { dataset_id; arn; kms_key_arn }

let parse xml =
  Some
    { dataset_id =
        Aws.Xml.required
          "DatasetId"
          (Aws.Util.option_bind (Aws.Xml.member "DatasetId" xml) String.parse)
    ; arn =
        Aws.Xml.required
          "Arn"
          (Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse)
    ; kms_key_arn = Aws.Util.option_bind (Aws.Xml.member "KmsKeyArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_arn (fun f ->
             Aws.Query.Pair ("KmsKeyArn", String.to_query f))
       ; Some (Aws.Query.Pair ("Arn", String.to_query v.arn))
       ; Some (Aws.Query.Pair ("DatasetId", String.to_query v.dataset_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_arn (fun f -> "KmsKeyArn", String.to_json f)
       ; Some ("Arn", String.to_json v.arn)
       ; Some ("DatasetId", String.to_json v.dataset_id)
       ])

let of_json j =
  { dataset_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DatasetId"))
  ; arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Arn"))
  ; kms_key_arn = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyArn") String.of_json
  }
