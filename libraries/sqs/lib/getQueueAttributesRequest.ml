open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; attribute_names : AttributeNameList.t
  }

let make ~queue_url ?(attribute_names = []) () = { queue_url; attribute_names }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; attribute_names = Aws.Util.of_option [] (AttributeNameList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AttributeNames.member", AttributeNameList.to_query v.attribute_names))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AttributeNames", AttributeNameList.to_json v.attribute_names)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; attribute_names =
      AttributeNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeNames"))
  }
