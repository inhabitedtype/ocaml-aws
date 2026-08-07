open Aws.BaseTypes

type t =
  { queue_name : String.t
  ; attributes : QueueAttributeMap.t option
  ; tags : TagMap.t option
  }

let make ~queue_name ?attributes ?tags () = { queue_name; attributes; tags }

let parse xml =
  Some
    { queue_name =
        Aws.Xml.required
          "QueueName"
          (Aws.Util.option_bind (Aws.Xml.member "QueueName" xml) String.parse)
    ; attributes =
        Aws.Util.option_bind (Aws.Xml.member "Attributes" xml) QueueAttributeMap.parse
    ; tags = Aws.Util.option_bind (Aws.Xml.member "tags" xml) TagMap.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tags (fun f -> Aws.Query.Pair ("tags", TagMap.to_query f))
       ; Aws.Util.option_map v.attributes (fun f ->
             Aws.Query.Pair ("Attributes", QueueAttributeMap.to_query f))
       ; Some (Aws.Query.Pair ("QueueName", String.to_query v.queue_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tags (fun f -> "tags", TagMap.to_json f)
       ; Aws.Util.option_map v.attributes (fun f ->
             "Attributes", QueueAttributeMap.to_json f)
       ; Some ("QueueName", String.to_json v.queue_name)
       ])

let of_json j =
  { queue_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueName"))
  ; attributes =
      Aws.Util.option_map (Aws.Json.lookup j "Attributes") QueueAttributeMap.of_json
  ; tags = Aws.Util.option_map (Aws.Json.lookup j "tags") TagMap.of_json
  }
