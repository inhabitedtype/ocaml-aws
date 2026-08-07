open Aws.BaseTypes

type t =
  | All
  | SenderId
  | SentTimestamp
  | ApproximateReceiveCount
  | ApproximateFirstReceiveTimestamp
  | SequenceNumber
  | MessageDeduplicationId
  | MessageGroupId
  | AWSTraceHeader
  | DeadLetterQueueSourceArn

let str_to_t =
  [ "DeadLetterQueueSourceArn", DeadLetterQueueSourceArn
  ; "AWSTraceHeader", AWSTraceHeader
  ; "MessageGroupId", MessageGroupId
  ; "MessageDeduplicationId", MessageDeduplicationId
  ; "SequenceNumber", SequenceNumber
  ; "ApproximateFirstReceiveTimestamp", ApproximateFirstReceiveTimestamp
  ; "ApproximateReceiveCount", ApproximateReceiveCount
  ; "SentTimestamp", SentTimestamp
  ; "SenderId", SenderId
  ; "All", All
  ]

let t_to_str =
  [ DeadLetterQueueSourceArn, "DeadLetterQueueSourceArn"
  ; AWSTraceHeader, "AWSTraceHeader"
  ; MessageGroupId, "MessageGroupId"
  ; MessageDeduplicationId, "MessageDeduplicationId"
  ; SequenceNumber, "SequenceNumber"
  ; ApproximateFirstReceiveTimestamp, "ApproximateFirstReceiveTimestamp"
  ; ApproximateReceiveCount, "ApproximateReceiveCount"
  ; SentTimestamp, "SentTimestamp"
  ; SenderId, "SenderId"
  ; All, "All"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
