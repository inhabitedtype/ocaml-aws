open Aws.BaseTypes

type t =
  { subscription_name : String.t
  ; source_identifier : String.t
  }

let make ~subscription_name ~source_identifier () =
  { subscription_name; source_identifier }

let parse xml =
  Some
    { subscription_name =
        Aws.Xml.required
          "SubscriptionName"
          (Aws.Util.option_bind (Aws.Xml.member "SubscriptionName" xml) String.parse)
    ; source_identifier =
        Aws.Xml.required
          "SourceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "SourceIdentifier" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("SourceIdentifier", String.to_query v.source_identifier))
       ; Some (Aws.Query.Pair ("SubscriptionName", String.to_query v.subscription_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SourceIdentifier", String.to_json v.source_identifier)
       ; Some ("SubscriptionName", String.to_json v.subscription_name)
       ])

let of_json j =
  { subscription_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubscriptionName"))
  ; source_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceIdentifier"))
  }
