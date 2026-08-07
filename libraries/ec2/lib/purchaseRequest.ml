open Aws.BaseTypes

type t =
  { instance_count : Integer.t
  ; purchase_token : String.t
  }

let make ~instance_count ~purchase_token () = { instance_count; purchase_token }

let parse xml =
  Some
    { instance_count =
        Aws.Xml.required
          "InstanceCount"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse)
    ; purchase_token =
        Aws.Xml.required
          "PurchaseToken"
          (Aws.Util.option_bind (Aws.Xml.member "PurchaseToken" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PurchaseToken", String.to_query v.purchase_token))
       ; Some (Aws.Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PurchaseToken", String.to_json v.purchase_token)
       ; Some ("InstanceCount", Integer.to_json v.instance_count)
       ])

let of_json j =
  { instance_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceCount"))
  ; purchase_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PurchaseToken"))
  }
