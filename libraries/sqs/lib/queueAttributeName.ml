open Aws.BaseTypes

type t =
  | All
  | Policy
  | VisibilityTimeout
  | MaximumMessageSize
  | MessageRetentionPeriod
  | ApproximateNumberOfMessages
  | ApproximateNumberOfMessagesNotVisible
  | CreatedTimestamp
  | LastModifiedTimestamp
  | QueueArn
  | ApproximateNumberOfMessagesDelayed
  | DelaySeconds
  | ReceiveMessageWaitTimeSeconds
  | RedrivePolicy
  | FifoQueue
  | ContentBasedDeduplication
  | KmsMasterKeyId
  | KmsDataKeyReusePeriodSeconds
  | DeduplicationScope
  | FifoThroughputLimit
  | RedriveAllowPolicy
  | SqsManagedSseEnabled

let str_to_t =
  [ "SqsManagedSseEnabled", SqsManagedSseEnabled
  ; "RedriveAllowPolicy", RedriveAllowPolicy
  ; "FifoThroughputLimit", FifoThroughputLimit
  ; "DeduplicationScope", DeduplicationScope
  ; "KmsDataKeyReusePeriodSeconds", KmsDataKeyReusePeriodSeconds
  ; "KmsMasterKeyId", KmsMasterKeyId
  ; "ContentBasedDeduplication", ContentBasedDeduplication
  ; "FifoQueue", FifoQueue
  ; "RedrivePolicy", RedrivePolicy
  ; "ReceiveMessageWaitTimeSeconds", ReceiveMessageWaitTimeSeconds
  ; "DelaySeconds", DelaySeconds
  ; "ApproximateNumberOfMessagesDelayed", ApproximateNumberOfMessagesDelayed
  ; "QueueArn", QueueArn
  ; "LastModifiedTimestamp", LastModifiedTimestamp
  ; "CreatedTimestamp", CreatedTimestamp
  ; "ApproximateNumberOfMessagesNotVisible", ApproximateNumberOfMessagesNotVisible
  ; "ApproximateNumberOfMessages", ApproximateNumberOfMessages
  ; "MessageRetentionPeriod", MessageRetentionPeriod
  ; "MaximumMessageSize", MaximumMessageSize
  ; "VisibilityTimeout", VisibilityTimeout
  ; "Policy", Policy
  ; "All", All
  ]

let t_to_str =
  [ SqsManagedSseEnabled, "SqsManagedSseEnabled"
  ; RedriveAllowPolicy, "RedriveAllowPolicy"
  ; FifoThroughputLimit, "FifoThroughputLimit"
  ; DeduplicationScope, "DeduplicationScope"
  ; KmsDataKeyReusePeriodSeconds, "KmsDataKeyReusePeriodSeconds"
  ; KmsMasterKeyId, "KmsMasterKeyId"
  ; ContentBasedDeduplication, "ContentBasedDeduplication"
  ; FifoQueue, "FifoQueue"
  ; RedrivePolicy, "RedrivePolicy"
  ; ReceiveMessageWaitTimeSeconds, "ReceiveMessageWaitTimeSeconds"
  ; DelaySeconds, "DelaySeconds"
  ; ApproximateNumberOfMessagesDelayed, "ApproximateNumberOfMessagesDelayed"
  ; QueueArn, "QueueArn"
  ; LastModifiedTimestamp, "LastModifiedTimestamp"
  ; CreatedTimestamp, "CreatedTimestamp"
  ; ApproximateNumberOfMessagesNotVisible, "ApproximateNumberOfMessagesNotVisible"
  ; ApproximateNumberOfMessages, "ApproximateNumberOfMessages"
  ; MessageRetentionPeriod, "MessageRetentionPeriod"
  ; MaximumMessageSize, "MaximumMessageSize"
  ; VisibilityTimeout, "VisibilityTimeout"
  ; Policy, "Policy"
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
