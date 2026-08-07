type t =
  | AuthFailure
  | BatchEntryIdsNotDistinct
  | BatchRequestTooLong
  | Blocked
  | DryRunOperation
  | EmptyBatchRequest
  | IdempotentParameterMismatch
  | IncompleteSignature
  | InternalFailure
  | InvalidAction
  | InvalidAddress
  | InvalidAttributeName
  | InvalidAttributeValue
  | InvalidBatchEntryId
  | InvalidClientTokenId
  | InvalidIdFormat
  | InvalidMessageContents
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidSecurity
  | KmsAccessDenied
  | KmsDisabled
  | KmsInvalidKeyUsage
  | KmsInvalidState
  | KmsNotFound
  | KmsOptInRequired
  | KmsThrottled
  | MalformedQueryString
  | MessageNotInflight
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | OptInRequired
  | OverLimit
  | PendingVerification
  | PurgeQueueInProgress
  | QueueDeletedRecently
  | QueueDoesNotExist
  | QueueNameExists
  | ReceiptHandleIsInvalid
  | RequestExpired
  | RequestLimitExceeded
  | RequestThrottled
  | ResourceNotFoundException
  | ServiceUnavailable
  | Throttling
  | TooManyEntriesInBatchRequest
  | UnauthorizedOperation
  | UnknownParameter
  | UnsupportedOperation
  | UnsupportedProtocol
  | ValidationError
  | Uninhabited

let common =
  [ UnsupportedProtocol
  ; UnknownParameter
  ; UnauthorizedOperation
  ; RequestLimitExceeded
  ; PendingVerification
  ; InvalidParameter
  ; IdempotentParameterMismatch
  ; DryRunOperation
  ; Blocked
  ; AuthFailure
  ; ValidationError
  ; Throttling
  ; ServiceUnavailable
  ; RequestExpired
  ; OptInRequired
  ; MissingParameter
  ; MissingAuthenticationToken
  ; MissingAction
  ; MalformedQueryString
  ; InvalidQueryParameter
  ; InvalidParameterValue
  ; InvalidParameterCombination
  ; InvalidClientTokenId
  ; InvalidAction
  ; InternalFailure
  ; IncompleteSignature
  ]

let to_http_code e =
  match e with
  | AuthFailure -> None
  | BatchEntryIdsNotDistinct -> None
  | BatchRequestTooLong -> None
  | Blocked -> None
  | DryRunOperation -> None
  | EmptyBatchRequest -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidAddress -> None
  | InvalidAttributeName -> None
  | InvalidAttributeValue -> None
  | InvalidBatchEntryId -> None
  | InvalidClientTokenId -> Some 403
  | InvalidIdFormat -> None
  | InvalidMessageContents -> None
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidSecurity -> None
  | KmsAccessDenied -> None
  | KmsDisabled -> None
  | KmsInvalidKeyUsage -> None
  | KmsInvalidState -> None
  | KmsNotFound -> None
  | KmsOptInRequired -> None
  | KmsThrottled -> None
  | MalformedQueryString -> Some 404
  | MessageNotInflight -> None
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OptInRequired -> Some 403
  | OverLimit -> None
  | PendingVerification -> None
  | PurgeQueueInProgress -> None
  | QueueDeletedRecently -> None
  | QueueDoesNotExist -> None
  | QueueNameExists -> None
  | ReceiptHandleIsInvalid -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | RequestThrottled -> None
  | ResourceNotFoundException -> None
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | TooManyEntriesInBatchRequest -> None
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedOperation -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | BatchEntryIdsNotDistinct -> "BatchEntryIdsNotDistinct"
  | BatchRequestTooLong -> "BatchRequestTooLong"
  | Blocked -> "Blocked"
  | DryRunOperation -> "DryRunOperation"
  | EmptyBatchRequest -> "EmptyBatchRequest"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidAddress -> "InvalidAddress"
  | InvalidAttributeName -> "InvalidAttributeName"
  | InvalidAttributeValue -> "InvalidAttributeValue"
  | InvalidBatchEntryId -> "InvalidBatchEntryId"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidIdFormat -> "InvalidIdFormat"
  | InvalidMessageContents -> "InvalidMessageContents"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidSecurity -> "InvalidSecurity"
  | KmsAccessDenied -> "KmsAccessDenied"
  | KmsDisabled -> "KmsDisabled"
  | KmsInvalidKeyUsage -> "KmsInvalidKeyUsage"
  | KmsInvalidState -> "KmsInvalidState"
  | KmsNotFound -> "KmsNotFound"
  | KmsOptInRequired -> "KmsOptInRequired"
  | KmsThrottled -> "KmsThrottled"
  | MalformedQueryString -> "MalformedQueryString"
  | MessageNotInflight -> "MessageNotInflight"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OptInRequired -> "OptInRequired"
  | OverLimit -> "OverLimit"
  | PendingVerification -> "PendingVerification"
  | PurgeQueueInProgress -> "PurgeQueueInProgress"
  | QueueDeletedRecently -> "QueueDeletedRecently"
  | QueueDoesNotExist -> "QueueDoesNotExist"
  | QueueNameExists -> "QueueNameExists"
  | ReceiptHandleIsInvalid -> "ReceiptHandleIsInvalid"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | RequestThrottled -> "RequestThrottled"
  | ResourceNotFoundException -> "ResourceNotFoundException"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | TooManyEntriesInBatchRequest -> "TooManyEntriesInBatchRequest"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedOperation -> "UnsupportedOperation"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "BatchEntryIdsNotDistinct" -> Some BatchEntryIdsNotDistinct
  | "BatchRequestTooLong" -> Some BatchRequestTooLong
  | "Blocked" -> Some Blocked
  | "DryRunOperation" -> Some DryRunOperation
  | "EmptyBatchRequest" -> Some EmptyBatchRequest
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidAddress" -> Some InvalidAddress
  | "InvalidAttributeName" -> Some InvalidAttributeName
  | "InvalidAttributeValue" -> Some InvalidAttributeValue
  | "InvalidBatchEntryId" -> Some InvalidBatchEntryId
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidIdFormat" -> Some InvalidIdFormat
  | "InvalidMessageContents" -> Some InvalidMessageContents
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidSecurity" -> Some InvalidSecurity
  | "KmsAccessDenied" -> Some KmsAccessDenied
  | "KmsDisabled" -> Some KmsDisabled
  | "KmsInvalidKeyUsage" -> Some KmsInvalidKeyUsage
  | "KmsInvalidState" -> Some KmsInvalidState
  | "KmsNotFound" -> Some KmsNotFound
  | "KmsOptInRequired" -> Some KmsOptInRequired
  | "KmsThrottled" -> Some KmsThrottled
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MessageNotInflight" -> Some MessageNotInflight
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "OverLimit" -> Some OverLimit
  | "PendingVerification" -> Some PendingVerification
  | "PurgeQueueInProgress" -> Some PurgeQueueInProgress
  | "QueueDeletedRecently" -> Some QueueDeletedRecently
  | "QueueDoesNotExist" -> Some QueueDoesNotExist
  | "QueueNameExists" -> Some QueueNameExists
  | "ReceiptHandleIsInvalid" -> Some ReceiptHandleIsInvalid
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "RequestThrottled" -> Some RequestThrottled
  | "ResourceNotFoundException" -> Some ResourceNotFoundException
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "TooManyEntriesInBatchRequest" -> Some TooManyEntriesInBatchRequest
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedOperation" -> Some UnsupportedOperation
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
