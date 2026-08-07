open Aws.BaseTypes

type t =
  { customer_managed_key_enabled : Boolean.t option
  ; kms_key_arn : String.t option
  }

let make ?customer_managed_key_enabled ?kms_key_arn () =
  { customer_managed_key_enabled; kms_key_arn }

let parse xml =
  Some
    { customer_managed_key_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomerManagedKeyEnabled" xml)
          Boolean.parse
    ; kms_key_arn = Aws.Util.option_bind (Aws.Xml.member "KmsKeyArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_arn (fun f ->
             Aws.Query.Pair ("KmsKeyArn", String.to_query f))
       ; Aws.Util.option_map v.customer_managed_key_enabled (fun f ->
             Aws.Query.Pair ("CustomerManagedKeyEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_arn (fun f -> "KmsKeyArn", String.to_json f)
       ; Aws.Util.option_map v.customer_managed_key_enabled (fun f ->
             "CustomerManagedKeyEnabled", Boolean.to_json f)
       ])

let of_json j =
  { customer_managed_key_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerManagedKeyEnabled") Boolean.of_json
  ; kms_key_arn = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyArn") String.of_json
  }
