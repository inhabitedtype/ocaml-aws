open Aws.BaseTypes

type t =
  { topic_arn : String.t option
  ; topic_status : String.t option
  }

let make ?topic_arn ?topic_status () = { topic_arn; topic_status }

let parse xml =
  Some
    { topic_arn = Aws.Util.option_bind (Aws.Xml.member "TopicArn" xml) String.parse
    ; topic_status = Aws.Util.option_bind (Aws.Xml.member "TopicStatus" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.topic_status (fun f ->
             Aws.Query.Pair ("TopicStatus", String.to_query f))
       ; Aws.Util.option_map v.topic_arn (fun f ->
             Aws.Query.Pair ("TopicArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.topic_status (fun f -> "TopicStatus", String.to_json f)
       ; Aws.Util.option_map v.topic_arn (fun f -> "TopicArn", String.to_json f)
       ])

let of_json j =
  { topic_arn = Aws.Util.option_map (Aws.Json.lookup j "TopicArn") String.of_json
  ; topic_status = Aws.Util.option_map (Aws.Json.lookup j "TopicStatus") String.of_json
  }
