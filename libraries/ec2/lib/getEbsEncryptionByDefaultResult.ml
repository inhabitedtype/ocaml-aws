open Aws.BaseTypes

type t =
  { ebs_encryption_by_default : Boolean.t option
  ; sse_type : SSEType.t option
  }

let make ?ebs_encryption_by_default ?sse_type () = { ebs_encryption_by_default; sse_type }

let parse xml =
  Some
    { ebs_encryption_by_default =
        Aws.Util.option_bind (Aws.Xml.member "ebsEncryptionByDefault" xml) Boolean.parse
    ; sse_type = Aws.Util.option_bind (Aws.Xml.member "sseType" xml) SSEType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_type (fun f ->
             Aws.Query.Pair ("SseType", SSEType.to_query f))
       ; Aws.Util.option_map v.ebs_encryption_by_default (fun f ->
             Aws.Query.Pair ("EbsEncryptionByDefault", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_type (fun f -> "sseType", SSEType.to_json f)
       ; Aws.Util.option_map v.ebs_encryption_by_default (fun f ->
             "ebsEncryptionByDefault", Boolean.to_json f)
       ])

let of_json j =
  { ebs_encryption_by_default =
      Aws.Util.option_map (Aws.Json.lookup j "ebsEncryptionByDefault") Boolean.of_json
  ; sse_type = Aws.Util.option_map (Aws.Json.lookup j "sseType") SSEType.of_json
  }
