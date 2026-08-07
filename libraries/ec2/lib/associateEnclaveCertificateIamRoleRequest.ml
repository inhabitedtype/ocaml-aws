open Aws.BaseTypes

type t =
  { certificate_arn : String.t
  ; role_arn : String.t
  ; dry_run : Boolean.t option
  }

let make ~certificate_arn ~role_arn ?dry_run () = { certificate_arn; role_arn; dry_run }

let parse xml =
  Some
    { certificate_arn =
        Aws.Xml.required
          "CertificateArn"
          (Aws.Util.option_bind (Aws.Xml.member "CertificateArn" xml) String.parse)
    ; role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ; Some (Aws.Query.Pair ("CertificateArn", String.to_query v.certificate_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ; Some ("CertificateArn", String.to_json v.certificate_arn)
       ])

let of_json j =
  { certificate_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CertificateArn"))
  ; role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
