open Aws.BaseTypes

type t = { queue_url : String.t option }

let make ?queue_url () = { queue_url }

let parse xml =
  Some { queue_url = Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.queue_url (fun f ->
             Aws.Query.Pair ("QueueUrl", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.queue_url (fun f -> "QueueUrl", String.to_json f) ])

let of_json j =
  { queue_url = Aws.Util.option_map (Aws.Json.lookup j "QueueUrl") String.of_json }
