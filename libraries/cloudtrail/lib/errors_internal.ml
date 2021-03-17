type t =
  | AuthFailure 
  | Blocked 
  | CloudTrailARNInvalidException 
  | CloudTrailAccessNotEnabledException 
  | CloudWatchLogsDeliveryUnavailableException 
  | DryRunOperation 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InsightNotEnabledException 
  | InsufficientDependencyServiceAccessPermissionException 
  | InsufficientEncryptionPolicyException 
  | InsufficientS3BucketPolicyException 
  | InsufficientSnsTopicPolicyException 
  | InternalFailure 
  | InvalidAction 
  | InvalidClientTokenId 
  | InvalidCloudWatchLogsLogGroupArnException 
  | InvalidCloudWatchLogsRoleArnException 
  | InvalidEventCategoryException 
  | InvalidEventSelectorsException 
  | InvalidHomeRegionException 
  | InvalidInsightSelectorsException 
  | InvalidKmsKeyIdException 
  | InvalidLookupAttributesException 
  | InvalidMaxResultsException 
  | InvalidNextTokenException 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterCombinationException 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | InvalidS3BucketNameException 
  | InvalidS3PrefixException 
  | InvalidSnsTopicNameException 
  | InvalidTagParameterException 
  | InvalidTimeRangeException 
  | InvalidTokenException 
  | InvalidTrailNameException 
  | KmsException 
  | KmsKeyDisabledException 
  | KmsKeyNotFoundException 
  | MalformedQueryString 
  | MaximumNumberOfTrailsExceededException 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | NotOrganizationMasterAccountException 
  | OperationNotPermittedException 
  | OptInRequired 
  | OrganizationNotInAllFeaturesModeException 
  | OrganizationsNotInUseException 
  | PendingVerification 
  | RequestExpired 
  | RequestLimitExceeded 
  | ResourceNotFoundException 
  | ResourceTypeNotSupportedException 
  | S3BucketDoesNotExistException 
  | ServiceUnavailable 
  | TagsLimitExceededException 
  | Throttling 
  | TrailAlreadyExistsException 
  | TrailNotFoundException 
  | TrailNotProvidedException 
  | UnauthorizedOperation 
  | UnknownParameter 
  | UnsupportedOperationException 
  | UnsupportedProtocol 
  | ValidationError 
  | Uninhabited 
let common =
  [UnsupportedProtocol;
  UnknownParameter;
  UnauthorizedOperation;
  RequestLimitExceeded;
  PendingVerification;
  InvalidParameter;
  IdempotentParameterMismatch;
  DryRunOperation;
  Blocked;
  AuthFailure;
  ValidationError;
  Throttling;
  ServiceUnavailable;
  RequestExpired;
  OptInRequired;
  MissingParameter;
  MissingAuthenticationToken;
  MissingAction;
  MalformedQueryString;
  InvalidQueryParameter;
  InvalidParameterValue;
  InvalidParameterCombination;
  InvalidClientTokenId;
  InvalidAction;
  InternalFailure;
  IncompleteSignature]
let to_http_code e =
  match e with
  | AuthFailure -> None
  | Blocked -> None
  | CloudTrailARNInvalidException -> None
  | CloudTrailAccessNotEnabledException -> None
  | CloudWatchLogsDeliveryUnavailableException -> None
  | DryRunOperation -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InsightNotEnabledException -> None
  | InsufficientDependencyServiceAccessPermissionException -> None
  | InsufficientEncryptionPolicyException -> None
  | InsufficientS3BucketPolicyException -> None
  | InsufficientSnsTopicPolicyException -> None
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidCloudWatchLogsLogGroupArnException -> None
  | InvalidCloudWatchLogsRoleArnException -> None
  | InvalidEventCategoryException -> None
  | InvalidEventSelectorsException -> None
  | InvalidHomeRegionException -> None
  | InvalidInsightSelectorsException -> None
  | InvalidKmsKeyIdException -> None
  | InvalidLookupAttributesException -> None
  | InvalidMaxResultsException -> None
  | InvalidNextTokenException -> None
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterCombinationException -> None
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidS3BucketNameException -> None
  | InvalidS3PrefixException -> None
  | InvalidSnsTopicNameException -> None
  | InvalidTagParameterException -> None
  | InvalidTimeRangeException -> None
  | InvalidTokenException -> None
  | InvalidTrailNameException -> None
  | KmsException -> None
  | KmsKeyDisabledException -> None
  | KmsKeyNotFoundException -> None
  | MalformedQueryString -> Some 404
  | MaximumNumberOfTrailsExceededException -> None
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NotOrganizationMasterAccountException -> None
  | OperationNotPermittedException -> None
  | OptInRequired -> Some 403
  | OrganizationNotInAllFeaturesModeException -> None
  | OrganizationsNotInUseException -> None
  | PendingVerification -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ResourceNotFoundException -> None
  | ResourceTypeNotSupportedException -> None
  | S3BucketDoesNotExistException -> None
  | ServiceUnavailable -> Some 503
  | TagsLimitExceededException -> None
  | Throttling -> Some 400
  | TrailAlreadyExistsException -> None
  | TrailNotFoundException -> None
  | TrailNotProvidedException -> None
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedOperationException -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | CloudTrailARNInvalidException -> "CloudTrailARNInvalidException"
  | CloudTrailAccessNotEnabledException ->
      "CloudTrailAccessNotEnabledException"
  | CloudWatchLogsDeliveryUnavailableException ->
      "CloudWatchLogsDeliveryUnavailableException"
  | DryRunOperation -> "DryRunOperation"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InsightNotEnabledException -> "InsightNotEnabledException"
  | InsufficientDependencyServiceAccessPermissionException ->
      "InsufficientDependencyServiceAccessPermissionException"
  | InsufficientEncryptionPolicyException ->
      "InsufficientEncryptionPolicyException"
  | InsufficientS3BucketPolicyException ->
      "InsufficientS3BucketPolicyException"
  | InsufficientSnsTopicPolicyException ->
      "InsufficientSnsTopicPolicyException"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidCloudWatchLogsLogGroupArnException ->
      "InvalidCloudWatchLogsLogGroupArnException"
  | InvalidCloudWatchLogsRoleArnException ->
      "InvalidCloudWatchLogsRoleArnException"
  | InvalidEventCategoryException -> "InvalidEventCategoryException"
  | InvalidEventSelectorsException -> "InvalidEventSelectorsException"
  | InvalidHomeRegionException -> "InvalidHomeRegionException"
  | InvalidInsightSelectorsException -> "InvalidInsightSelectorsException"
  | InvalidKmsKeyIdException -> "InvalidKmsKeyIdException"
  | InvalidLookupAttributesException -> "InvalidLookupAttributesException"
  | InvalidMaxResultsException -> "InvalidMaxResultsException"
  | InvalidNextTokenException -> "InvalidNextTokenException"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterCombinationException ->
      "InvalidParameterCombinationException"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidS3BucketNameException -> "InvalidS3BucketNameException"
  | InvalidS3PrefixException -> "InvalidS3PrefixException"
  | InvalidSnsTopicNameException -> "InvalidSnsTopicNameException"
  | InvalidTagParameterException -> "InvalidTagParameterException"
  | InvalidTimeRangeException -> "InvalidTimeRangeException"
  | InvalidTokenException -> "InvalidTokenException"
  | InvalidTrailNameException -> "InvalidTrailNameException"
  | KmsException -> "KmsException"
  | KmsKeyDisabledException -> "KmsKeyDisabledException"
  | KmsKeyNotFoundException -> "KmsKeyNotFoundException"
  | MalformedQueryString -> "MalformedQueryString"
  | MaximumNumberOfTrailsExceededException ->
      "MaximumNumberOfTrailsExceededException"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NotOrganizationMasterAccountException ->
      "NotOrganizationMasterAccountException"
  | OperationNotPermittedException -> "OperationNotPermittedException"
  | OptInRequired -> "OptInRequired"
  | OrganizationNotInAllFeaturesModeException ->
      "OrganizationNotInAllFeaturesModeException"
  | OrganizationsNotInUseException -> "OrganizationsNotInUseException"
  | PendingVerification -> "PendingVerification"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ResourceNotFoundException -> "ResourceNotFoundException"
  | ResourceTypeNotSupportedException -> "ResourceTypeNotSupportedException"
  | S3BucketDoesNotExistException -> "S3BucketDoesNotExistException"
  | ServiceUnavailable -> "ServiceUnavailable"
  | TagsLimitExceededException -> "TagsLimitExceededException"
  | Throttling -> "Throttling"
  | TrailAlreadyExistsException -> "TrailAlreadyExistsException"
  | TrailNotFoundException -> "TrailNotFoundException"
  | TrailNotProvidedException -> "TrailNotProvidedException"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedOperationException -> "UnsupportedOperationException"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CloudTrailARNInvalidException" -> Some CloudTrailARNInvalidException
  | "CloudTrailAccessNotEnabledException" ->
      Some CloudTrailAccessNotEnabledException
  | "CloudWatchLogsDeliveryUnavailableException" ->
      Some CloudWatchLogsDeliveryUnavailableException
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InsightNotEnabledException" -> Some InsightNotEnabledException
  | "InsufficientDependencyServiceAccessPermissionException" ->
      Some InsufficientDependencyServiceAccessPermissionException
  | "InsufficientEncryptionPolicyException" ->
      Some InsufficientEncryptionPolicyException
  | "InsufficientS3BucketPolicyException" ->
      Some InsufficientS3BucketPolicyException
  | "InsufficientSnsTopicPolicyException" ->
      Some InsufficientSnsTopicPolicyException
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidCloudWatchLogsLogGroupArnException" ->
      Some InvalidCloudWatchLogsLogGroupArnException
  | "InvalidCloudWatchLogsRoleArnException" ->
      Some InvalidCloudWatchLogsRoleArnException
  | "InvalidEventCategoryException" -> Some InvalidEventCategoryException
  | "InvalidEventSelectorsException" -> Some InvalidEventSelectorsException
  | "InvalidHomeRegionException" -> Some InvalidHomeRegionException
  | "InvalidInsightSelectorsException" ->
      Some InvalidInsightSelectorsException
  | "InvalidKmsKeyIdException" -> Some InvalidKmsKeyIdException
  | "InvalidLookupAttributesException" ->
      Some InvalidLookupAttributesException
  | "InvalidMaxResultsException" -> Some InvalidMaxResultsException
  | "InvalidNextTokenException" -> Some InvalidNextTokenException
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterCombinationException" ->
      Some InvalidParameterCombinationException
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidS3BucketNameException" -> Some InvalidS3BucketNameException
  | "InvalidS3PrefixException" -> Some InvalidS3PrefixException
  | "InvalidSnsTopicNameException" -> Some InvalidSnsTopicNameException
  | "InvalidTagParameterException" -> Some InvalidTagParameterException
  | "InvalidTimeRangeException" -> Some InvalidTimeRangeException
  | "InvalidTokenException" -> Some InvalidTokenException
  | "InvalidTrailNameException" -> Some InvalidTrailNameException
  | "KmsException" -> Some KmsException
  | "KmsKeyDisabledException" -> Some KmsKeyDisabledException
  | "KmsKeyNotFoundException" -> Some KmsKeyNotFoundException
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MaximumNumberOfTrailsExceededException" ->
      Some MaximumNumberOfTrailsExceededException
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NotOrganizationMasterAccountException" ->
      Some NotOrganizationMasterAccountException
  | "OperationNotPermittedException" -> Some OperationNotPermittedException
  | "OptInRequired" -> Some OptInRequired
  | "OrganizationNotInAllFeaturesModeException" ->
      Some OrganizationNotInAllFeaturesModeException
  | "OrganizationsNotInUseException" -> Some OrganizationsNotInUseException
  | "PendingVerification" -> Some PendingVerification
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ResourceNotFoundException" -> Some ResourceNotFoundException
  | "ResourceTypeNotSupportedException" ->
      Some ResourceTypeNotSupportedException
  | "S3BucketDoesNotExistException" -> Some S3BucketDoesNotExistException
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "TagsLimitExceededException" -> Some TagsLimitExceededException
  | "Throttling" -> Some Throttling
  | "TrailAlreadyExistsException" -> Some TrailAlreadyExistsException
  | "TrailNotFoundException" -> Some TrailNotFoundException
  | "TrailNotProvidedException" -> Some TrailNotProvidedException
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedOperationException" -> Some UnsupportedOperationException
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None