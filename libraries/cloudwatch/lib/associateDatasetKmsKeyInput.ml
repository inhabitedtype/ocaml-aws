open Aws.BaseTypes

type t =
  { dataset_identifier : String.t
  ; kms_key_arn : String.t
  }

let make ~dataset_identifier ~kms_key_arn () = { dataset_identifier; kms_key_arn }

let parse xml =
  Some
    { dataset_identifier =
        Aws.Xml.required
          "DatasetIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DatasetIdentifier" xml) String.parse)
    ; kms_key_arn =
        Aws.Xml.required
          "KmsKeyArn"
          (Aws.Util.option_bind (Aws.Xml.member "KmsKeyArn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("KmsKeyArn", String.to_query v.kms_key_arn))
       ; Some (Aws.Query.Pair ("DatasetIdentifier", String.to_query v.dataset_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("KmsKeyArn", String.to_json v.kms_key_arn)
       ; Some ("DatasetIdentifier", String.to_json v.dataset_identifier)
       ])

let of_json j =
  { dataset_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DatasetIdentifier"))
  ; kms_key_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KmsKeyArn"))
  }
