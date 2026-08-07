open Aws.BaseTypes

type t =
  { id : String.t
  ; receipt_handle : String.t
  ; visibility_timeout : Integer.t option
  }

let make ~id ~receipt_handle ?visibility_timeout () =
  { id; receipt_handle; visibility_timeout }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; receipt_handle =
        Aws.Xml.required
          "ReceiptHandle"
          (Aws.Util.option_bind (Aws.Xml.member "ReceiptHandle" xml) String.parse)
    ; visibility_timeout =
        Aws.Util.option_bind (Aws.Xml.member "VisibilityTimeout" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.visibility_timeout (fun f ->
             Aws.Query.Pair ("VisibilityTimeout", Integer.to_query f))
       ; Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.visibility_timeout (fun f ->
             "VisibilityTimeout", Integer.to_json f)
       ; Some ("ReceiptHandle", String.to_json v.receipt_handle)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; receipt_handle =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
  ; visibility_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "VisibilityTimeout") Integer.of_json
  }
