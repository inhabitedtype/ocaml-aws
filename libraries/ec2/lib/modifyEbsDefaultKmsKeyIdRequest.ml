open Aws.BaseTypes

type t =
  { kms_key_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~kms_key_id ?dry_run () = { kms_key_id; dry_run }

let parse xml =
  Some
    { kms_key_id =
        Aws.Xml.required
          "KmsKeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("KmsKeyId", String.to_query v.kms_key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("KmsKeyId", String.to_json v.kms_key_id)
       ])

let of_json j =
  { kms_key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KmsKeyId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
