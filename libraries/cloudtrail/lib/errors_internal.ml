type t =
  | AuthFailure
  | Blocked
  | CloudWatchLogsDeliveryUnavailable
  | DryRunOperation
  | IdempotentParameterMismatch
  | IncompleteSignature
  | InsufficientS3BucketPolicy
  | InsufficientSnsTopicPolicy
  | InternalFailure
  | InvalidAction
  | InvalidClientTokenId
  | InvalidCloudWatchLogsLogGroupArn
  | InvalidCloudWatchLogsRoleArn
  | InvalidLookupAttributes
  | InvalidMaxResults
  | InvalidNextToken
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidS3BucketName
  | InvalidS3Prefix
  | InvalidSnsTopicName
  | InvalidTimeRange
  | InvalidTrailName
  | MalformedQueryString
  | MaximumNumberOfTrailsExceeded
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | OptInRequired
  | PendingVerification
  | RequestExpired
  | RequestLimitExceeded
  | S3BucketDoesNotExist
  | ServiceUnavailable
  | Throttling
  | TrailAlreadyExists
  | TrailNotFound
  | UnauthorizedOperation
  | UnknownParameter
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
  | Blocked -> None
  | CloudWatchLogsDeliveryUnavailable -> Some 400
  | DryRunOperation -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InsufficientS3BucketPolicy -> Some 403
  | InsufficientSnsTopicPolicy -> Some 403
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidCloudWatchLogsLogGroupArn -> Some 400
  | InvalidCloudWatchLogsRoleArn -> Some 400
  | InvalidLookupAttributes -> Some 400
  | InvalidMaxResults -> Some 400
  | InvalidNextToken -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidS3BucketName -> Some 400
  | InvalidS3Prefix -> Some 400
  | InvalidSnsTopicName -> Some 400
  | InvalidTimeRange -> Some 400
  | InvalidTrailName -> Some 400
  | MalformedQueryString -> Some 404
  | MaximumNumberOfTrailsExceeded -> Some 403
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | S3BucketDoesNotExist -> Some 404
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | TrailAlreadyExists -> Some 400
  | TrailNotFound -> Some 404
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | CloudWatchLogsDeliveryUnavailable -> "CloudWatchLogsDeliveryUnavailable"
  | DryRunOperation -> "DryRunOperation"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InsufficientS3BucketPolicy -> "InsufficientS3BucketPolicy"
  | InsufficientSnsTopicPolicy -> "InsufficientSnsTopicPolicy"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidCloudWatchLogsLogGroupArn -> "InvalidCloudWatchLogsLogGroupArn"
  | InvalidCloudWatchLogsRoleArn -> "InvalidCloudWatchLogsRoleArn"
  | InvalidLookupAttributes -> "InvalidLookupAttributes"
  | InvalidMaxResults -> "InvalidMaxResults"
  | InvalidNextToken -> "InvalidNextToken"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidS3BucketName -> "InvalidS3BucketName"
  | InvalidS3Prefix -> "InvalidS3Prefix"
  | InvalidSnsTopicName -> "InvalidSnsTopicName"
  | InvalidTimeRange -> "InvalidTimeRange"
  | InvalidTrailName -> "InvalidTrailName"
  | MalformedQueryString -> "MalformedQueryString"
  | MaximumNumberOfTrailsExceeded -> "MaximumNumberOfTrailsExceeded"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | S3BucketDoesNotExist -> "S3BucketDoesNotExist"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | TrailAlreadyExists -> "TrailAlreadyExists"
  | TrailNotFound -> "TrailNotFound"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CloudWatchLogsDeliveryUnavailable" -> Some CloudWatchLogsDeliveryUnavailable
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InsufficientS3BucketPolicy" -> Some InsufficientS3BucketPolicy
  | "InsufficientSnsTopicPolicy" -> Some InsufficientSnsTopicPolicy
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidCloudWatchLogsLogGroupArn" -> Some InvalidCloudWatchLogsLogGroupArn
  | "InvalidCloudWatchLogsRoleArn" -> Some InvalidCloudWatchLogsRoleArn
  | "InvalidLookupAttributes" -> Some InvalidLookupAttributes
  | "InvalidMaxResults" -> Some InvalidMaxResults
  | "InvalidNextToken" -> Some InvalidNextToken
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidS3BucketName" -> Some InvalidS3BucketName
  | "InvalidS3Prefix" -> Some InvalidS3Prefix
  | "InvalidSnsTopicName" -> Some InvalidSnsTopicName
  | "InvalidTimeRange" -> Some InvalidTimeRange
  | "InvalidTrailName" -> Some InvalidTrailName
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MaximumNumberOfTrailsExceeded" -> Some MaximumNumberOfTrailsExceeded
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "S3BucketDoesNotExist" -> Some S3BucketDoesNotExist
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "TrailAlreadyExists" -> Some TrailAlreadyExists
  | "TrailNotFound" -> Some TrailNotFound
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
