open Aws.BaseTypes

type t = { ebs_encryption_by_default : Boolean.t option }

let make ?ebs_encryption_by_default () = { ebs_encryption_by_default }

let parse xml =
  Some
    { ebs_encryption_by_default =
        Aws.Util.option_bind (Aws.Xml.member "ebsEncryptionByDefault" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_encryption_by_default (fun f ->
             Aws.Query.Pair ("EbsEncryptionByDefault", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_encryption_by_default (fun f ->
             "ebsEncryptionByDefault", Boolean.to_json f)
       ])

let of_json j =
  { ebs_encryption_by_default =
      Aws.Util.option_map (Aws.Json.lookup j "ebsEncryptionByDefault") Boolean.of_json
  }
