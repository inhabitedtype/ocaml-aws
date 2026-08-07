open Aws.BaseTypes

type t =
  { id : String.t
  ; receipt_handle : String.t
  }

let make ~id ~receipt_handle () = { id; receipt_handle }

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
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ReceiptHandle", String.to_query v.receipt_handle))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ReceiptHandle", String.to_json v.receipt_handle)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; receipt_handle =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReceiptHandle"))
  }
