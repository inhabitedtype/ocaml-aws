type t =
  | AccessDeniedException
  | AccountHasOngoingImportException
  | AccountNotFoundException
  | AccountNotRegisteredException
  | AccountRegisteredException
  | AuthFailure
  | Blocked
  | CannotDelegateManagementAccountException
  | ChannelARNInvalidException
  | ChannelAlreadyExistsException
  | ChannelExistsForEDSException
  | ChannelMaxLimitExceededException
  | ChannelNotFoundException
  | CloudTrailARNInvalidException
  | CloudTrailAccessNotEnabledException
  | CloudTrailInvalidClientTokenIdException
  | CloudWatchLogsDeliveryUnavailableException
  | ConcurrentModificationException
  | ConflictException
  | DelegatedAdminAccountLimitExceededException
  | DryRunOperation
  | EventDataStoreARNInvalidException
  | EventDataStoreAlreadyExistsException
  | EventDataStoreFederationEnabledException
  | EventDataStoreHasOngoingImportException
  | EventDataStoreMaxLimitExceededException
  | EventDataStoreNotFoundException
  | EventDataStoreTerminationProtectedException
  | GenerateResponseException
  | IdempotentParameterMismatch
  | ImportNotFoundException
  | InactiveEventDataStoreException
  | InactiveQueryException
  | IncompleteSignature
  | InsightNotEnabledException
  | InsufficientDependencyServiceAccessPermissionException
  | InsufficientEncryptionPolicyException
  | InsufficientIAMAccessPermissionException
  | InsufficientS3BucketPolicyException
  | InsufficientSnsTopicPolicyException
  | InternalFailure
  | InvalidAction
  | InvalidClientTokenId
  | InvalidCloudWatchLogsLogGroupArnException
  | InvalidCloudWatchLogsRoleArnException
  | InvalidDateRangeException
  | InvalidEventCategoryException
  | InvalidEventDataStoreCategoryException
  | InvalidEventDataStoreStatusException
  | InvalidEventSelectorsException
  | InvalidHomeRegionException
  | InvalidImportSourceException
  | InvalidInsightSelectorsException
  | InvalidKmsKeyIdException
  | InvalidLookupAttributesException
  | InvalidMaxResultsException
  | InvalidNextTokenException
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterCombinationException
  | InvalidParameterException
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidQueryStatementException
  | InvalidQueryStatusException
  | InvalidS3BucketNameException
  | InvalidS3PrefixException
  | InvalidSnsTopicNameException
  | InvalidSourceException
  | InvalidTagParameterException
  | InvalidTimeRangeException
  | InvalidTokenException
  | InvalidTrailNameException
  | KmsException
  | KmsKeyDisabledException
  | KmsKeyNotFoundException
  | MalformedQueryString
  | MaxConcurrentQueriesException
  | MaximumNumberOfTrailsExceededException
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | NoManagementAccountSLRExistsException
  | NotOrganizationManagementAccountException
  | NotOrganizationMasterAccountException
  | OperationNotPermittedException
  | OptInRequired
  | OrganizationNotInAllFeaturesModeException
  | OrganizationsNotInUseException
  | PendingVerification
  | QueryIdNotFoundException
  | RequestExpired
  | RequestLimitExceeded
  | ResourceARNNotValidException
  | ResourceNotFoundException
  | ResourcePolicyNotFoundException
  | ResourcePolicyNotValidException
  | ResourceTypeNotSupportedException
  | S3BucketDoesNotExistException
  | ServiceQuotaExceededException
  | ServiceUnavailable
  | TagsLimitExceededException
  | Throttling
  | ThrottlingException
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
  | AccessDeniedException -> None
  | AccountHasOngoingImportException -> None
  | AccountNotFoundException -> None
  | AccountNotRegisteredException -> None
  | AccountRegisteredException -> None
  | AuthFailure -> None
  | Blocked -> None
  | CannotDelegateManagementAccountException -> None
  | ChannelARNInvalidException -> None
  | ChannelAlreadyExistsException -> None
  | ChannelExistsForEDSException -> None
  | ChannelMaxLimitExceededException -> None
  | ChannelNotFoundException -> None
  | CloudTrailARNInvalidException -> None
  | CloudTrailAccessNotEnabledException -> None
  | CloudTrailInvalidClientTokenIdException -> None
  | CloudWatchLogsDeliveryUnavailableException -> None
  | ConcurrentModificationException -> None
  | ConflictException -> None
  | DelegatedAdminAccountLimitExceededException -> None
  | DryRunOperation -> None
  | EventDataStoreARNInvalidException -> None
  | EventDataStoreAlreadyExistsException -> None
  | EventDataStoreFederationEnabledException -> None
  | EventDataStoreHasOngoingImportException -> None
  | EventDataStoreMaxLimitExceededException -> None
  | EventDataStoreNotFoundException -> None
  | EventDataStoreTerminationProtectedException -> None
  | GenerateResponseException -> None
  | IdempotentParameterMismatch -> None
  | ImportNotFoundException -> None
  | InactiveEventDataStoreException -> None
  | InactiveQueryException -> None
  | IncompleteSignature -> Some 400
  | InsightNotEnabledException -> None
  | InsufficientDependencyServiceAccessPermissionException -> None
  | InsufficientEncryptionPolicyException -> None
  | InsufficientIAMAccessPermissionException -> None
  | InsufficientS3BucketPolicyException -> None
  | InsufficientSnsTopicPolicyException -> None
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidCloudWatchLogsLogGroupArnException -> None
  | InvalidCloudWatchLogsRoleArnException -> None
  | InvalidDateRangeException -> None
  | InvalidEventCategoryException -> None
  | InvalidEventDataStoreCategoryException -> None
  | InvalidEventDataStoreStatusException -> None
  | InvalidEventSelectorsException -> None
  | InvalidHomeRegionException -> None
  | InvalidImportSourceException -> None
  | InvalidInsightSelectorsException -> None
  | InvalidKmsKeyIdException -> None
  | InvalidLookupAttributesException -> None
  | InvalidMaxResultsException -> None
  | InvalidNextTokenException -> None
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterCombinationException -> None
  | InvalidParameterException -> None
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidQueryStatementException -> None
  | InvalidQueryStatusException -> None
  | InvalidS3BucketNameException -> None
  | InvalidS3PrefixException -> None
  | InvalidSnsTopicNameException -> None
  | InvalidSourceException -> None
  | InvalidTagParameterException -> None
  | InvalidTimeRangeException -> None
  | InvalidTokenException -> None
  | InvalidTrailNameException -> None
  | KmsException -> None
  | KmsKeyDisabledException -> None
  | KmsKeyNotFoundException -> None
  | MalformedQueryString -> Some 404
  | MaxConcurrentQueriesException -> None
  | MaximumNumberOfTrailsExceededException -> None
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NoManagementAccountSLRExistsException -> None
  | NotOrganizationManagementAccountException -> None
  | NotOrganizationMasterAccountException -> None
  | OperationNotPermittedException -> None
  | OptInRequired -> Some 403
  | OrganizationNotInAllFeaturesModeException -> None
  | OrganizationsNotInUseException -> None
  | PendingVerification -> None
  | QueryIdNotFoundException -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ResourceARNNotValidException -> None
  | ResourceNotFoundException -> None
  | ResourcePolicyNotFoundException -> None
  | ResourcePolicyNotValidException -> None
  | ResourceTypeNotSupportedException -> None
  | S3BucketDoesNotExistException -> None
  | ServiceQuotaExceededException -> None
  | ServiceUnavailable -> Some 503
  | TagsLimitExceededException -> None
  | Throttling -> Some 400
  | ThrottlingException -> None
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
  | AccessDeniedException -> "AccessDeniedException"
  | AccountHasOngoingImportException -> "AccountHasOngoingImportException"
  | AccountNotFoundException -> "AccountNotFoundException"
  | AccountNotRegisteredException -> "AccountNotRegisteredException"
  | AccountRegisteredException -> "AccountRegisteredException"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | CannotDelegateManagementAccountException -> "CannotDelegateManagementAccountException"
  | ChannelARNInvalidException -> "ChannelARNInvalidException"
  | ChannelAlreadyExistsException -> "ChannelAlreadyExistsException"
  | ChannelExistsForEDSException -> "ChannelExistsForEDSException"
  | ChannelMaxLimitExceededException -> "ChannelMaxLimitExceededException"
  | ChannelNotFoundException -> "ChannelNotFoundException"
  | CloudTrailARNInvalidException -> "CloudTrailARNInvalidException"
  | CloudTrailAccessNotEnabledException -> "CloudTrailAccessNotEnabledException"
  | CloudTrailInvalidClientTokenIdException -> "CloudTrailInvalidClientTokenIdException"
  | CloudWatchLogsDeliveryUnavailableException ->
      "CloudWatchLogsDeliveryUnavailableException"
  | ConcurrentModificationException -> "ConcurrentModificationException"
  | ConflictException -> "ConflictException"
  | DelegatedAdminAccountLimitExceededException ->
      "DelegatedAdminAccountLimitExceededException"
  | DryRunOperation -> "DryRunOperation"
  | EventDataStoreARNInvalidException -> "EventDataStoreARNInvalidException"
  | EventDataStoreAlreadyExistsException -> "EventDataStoreAlreadyExistsException"
  | EventDataStoreFederationEnabledException -> "EventDataStoreFederationEnabledException"
  | EventDataStoreHasOngoingImportException -> "EventDataStoreHasOngoingImportException"
  | EventDataStoreMaxLimitExceededException -> "EventDataStoreMaxLimitExceededException"
  | EventDataStoreNotFoundException -> "EventDataStoreNotFoundException"
  | EventDataStoreTerminationProtectedException ->
      "EventDataStoreTerminationProtectedException"
  | GenerateResponseException -> "GenerateResponseException"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | ImportNotFoundException -> "ImportNotFoundException"
  | InactiveEventDataStoreException -> "InactiveEventDataStoreException"
  | InactiveQueryException -> "InactiveQueryException"
  | IncompleteSignature -> "IncompleteSignature"
  | InsightNotEnabledException -> "InsightNotEnabledException"
  | InsufficientDependencyServiceAccessPermissionException ->
      "InsufficientDependencyServiceAccessPermissionException"
  | InsufficientEncryptionPolicyException -> "InsufficientEncryptionPolicyException"
  | InsufficientIAMAccessPermissionException -> "InsufficientIAMAccessPermissionException"
  | InsufficientS3BucketPolicyException -> "InsufficientS3BucketPolicyException"
  | InsufficientSnsTopicPolicyException -> "InsufficientSnsTopicPolicyException"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidCloudWatchLogsLogGroupArnException ->
      "InvalidCloudWatchLogsLogGroupArnException"
  | InvalidCloudWatchLogsRoleArnException -> "InvalidCloudWatchLogsRoleArnException"
  | InvalidDateRangeException -> "InvalidDateRangeException"
  | InvalidEventCategoryException -> "InvalidEventCategoryException"
  | InvalidEventDataStoreCategoryException -> "InvalidEventDataStoreCategoryException"
  | InvalidEventDataStoreStatusException -> "InvalidEventDataStoreStatusException"
  | InvalidEventSelectorsException -> "InvalidEventSelectorsException"
  | InvalidHomeRegionException -> "InvalidHomeRegionException"
  | InvalidImportSourceException -> "InvalidImportSourceException"
  | InvalidInsightSelectorsException -> "InvalidInsightSelectorsException"
  | InvalidKmsKeyIdException -> "InvalidKmsKeyIdException"
  | InvalidLookupAttributesException -> "InvalidLookupAttributesException"
  | InvalidMaxResultsException -> "InvalidMaxResultsException"
  | InvalidNextTokenException -> "InvalidNextTokenException"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterCombinationException -> "InvalidParameterCombinationException"
  | InvalidParameterException -> "InvalidParameterException"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidQueryStatementException -> "InvalidQueryStatementException"
  | InvalidQueryStatusException -> "InvalidQueryStatusException"
  | InvalidS3BucketNameException -> "InvalidS3BucketNameException"
  | InvalidS3PrefixException -> "InvalidS3PrefixException"
  | InvalidSnsTopicNameException -> "InvalidSnsTopicNameException"
  | InvalidSourceException -> "InvalidSourceException"
  | InvalidTagParameterException -> "InvalidTagParameterException"
  | InvalidTimeRangeException -> "InvalidTimeRangeException"
  | InvalidTokenException -> "InvalidTokenException"
  | InvalidTrailNameException -> "InvalidTrailNameException"
  | KmsException -> "KmsException"
  | KmsKeyDisabledException -> "KmsKeyDisabledException"
  | KmsKeyNotFoundException -> "KmsKeyNotFoundException"
  | MalformedQueryString -> "MalformedQueryString"
  | MaxConcurrentQueriesException -> "MaxConcurrentQueriesException"
  | MaximumNumberOfTrailsExceededException -> "MaximumNumberOfTrailsExceededException"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NoManagementAccountSLRExistsException -> "NoManagementAccountSLRExistsException"
  | NotOrganizationManagementAccountException ->
      "NotOrganizationManagementAccountException"
  | NotOrganizationMasterAccountException -> "NotOrganizationMasterAccountException"
  | OperationNotPermittedException -> "OperationNotPermittedException"
  | OptInRequired -> "OptInRequired"
  | OrganizationNotInAllFeaturesModeException ->
      "OrganizationNotInAllFeaturesModeException"
  | OrganizationsNotInUseException -> "OrganizationsNotInUseException"
  | PendingVerification -> "PendingVerification"
  | QueryIdNotFoundException -> "QueryIdNotFoundException"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ResourceARNNotValidException -> "ResourceARNNotValidException"
  | ResourceNotFoundException -> "ResourceNotFoundException"
  | ResourcePolicyNotFoundException -> "ResourcePolicyNotFoundException"
  | ResourcePolicyNotValidException -> "ResourcePolicyNotValidException"
  | ResourceTypeNotSupportedException -> "ResourceTypeNotSupportedException"
  | S3BucketDoesNotExistException -> "S3BucketDoesNotExistException"
  | ServiceQuotaExceededException -> "ServiceQuotaExceededException"
  | ServiceUnavailable -> "ServiceUnavailable"
  | TagsLimitExceededException -> "TagsLimitExceededException"
  | Throttling -> "Throttling"
  | ThrottlingException -> "ThrottlingException"
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
  | "AccessDeniedException" -> Some AccessDeniedException
  | "AccountHasOngoingImportException" -> Some AccountHasOngoingImportException
  | "AccountNotFoundException" -> Some AccountNotFoundException
  | "AccountNotRegisteredException" -> Some AccountNotRegisteredException
  | "AccountRegisteredException" -> Some AccountRegisteredException
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CannotDelegateManagementAccountException" ->
      Some CannotDelegateManagementAccountException
  | "ChannelARNInvalidException" -> Some ChannelARNInvalidException
  | "ChannelAlreadyExistsException" -> Some ChannelAlreadyExistsException
  | "ChannelExistsForEDSException" -> Some ChannelExistsForEDSException
  | "ChannelMaxLimitExceededException" -> Some ChannelMaxLimitExceededException
  | "ChannelNotFoundException" -> Some ChannelNotFoundException
  | "CloudTrailARNInvalidException" -> Some CloudTrailARNInvalidException
  | "CloudTrailAccessNotEnabledException" -> Some CloudTrailAccessNotEnabledException
  | "CloudTrailInvalidClientTokenIdException" ->
      Some CloudTrailInvalidClientTokenIdException
  | "CloudWatchLogsDeliveryUnavailableException" ->
      Some CloudWatchLogsDeliveryUnavailableException
  | "ConcurrentModificationException" -> Some ConcurrentModificationException
  | "ConflictException" -> Some ConflictException
  | "DelegatedAdminAccountLimitExceededException" ->
      Some DelegatedAdminAccountLimitExceededException
  | "DryRunOperation" -> Some DryRunOperation
  | "EventDataStoreARNInvalidException" -> Some EventDataStoreARNInvalidException
  | "EventDataStoreAlreadyExistsException" -> Some EventDataStoreAlreadyExistsException
  | "EventDataStoreFederationEnabledException" ->
      Some EventDataStoreFederationEnabledException
  | "EventDataStoreHasOngoingImportException" ->
      Some EventDataStoreHasOngoingImportException
  | "EventDataStoreMaxLimitExceededException" ->
      Some EventDataStoreMaxLimitExceededException
  | "EventDataStoreNotFoundException" -> Some EventDataStoreNotFoundException
  | "EventDataStoreTerminationProtectedException" ->
      Some EventDataStoreTerminationProtectedException
  | "GenerateResponseException" -> Some GenerateResponseException
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "ImportNotFoundException" -> Some ImportNotFoundException
  | "InactiveEventDataStoreException" -> Some InactiveEventDataStoreException
  | "InactiveQueryException" -> Some InactiveQueryException
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InsightNotEnabledException" -> Some InsightNotEnabledException
  | "InsufficientDependencyServiceAccessPermissionException" ->
      Some InsufficientDependencyServiceAccessPermissionException
  | "InsufficientEncryptionPolicyException" -> Some InsufficientEncryptionPolicyException
  | "InsufficientIAMAccessPermissionException" ->
      Some InsufficientIAMAccessPermissionException
  | "InsufficientS3BucketPolicyException" -> Some InsufficientS3BucketPolicyException
  | "InsufficientSnsTopicPolicyException" -> Some InsufficientSnsTopicPolicyException
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidCloudWatchLogsLogGroupArnException" ->
      Some InvalidCloudWatchLogsLogGroupArnException
  | "InvalidCloudWatchLogsRoleArnException" -> Some InvalidCloudWatchLogsRoleArnException
  | "InvalidDateRangeException" -> Some InvalidDateRangeException
  | "InvalidEventCategoryException" -> Some InvalidEventCategoryException
  | "InvalidEventDataStoreCategoryException" ->
      Some InvalidEventDataStoreCategoryException
  | "InvalidEventDataStoreStatusException" -> Some InvalidEventDataStoreStatusException
  | "InvalidEventSelectorsException" -> Some InvalidEventSelectorsException
  | "InvalidHomeRegionException" -> Some InvalidHomeRegionException
  | "InvalidImportSourceException" -> Some InvalidImportSourceException
  | "InvalidInsightSelectorsException" -> Some InvalidInsightSelectorsException
  | "InvalidKmsKeyIdException" -> Some InvalidKmsKeyIdException
  | "InvalidLookupAttributesException" -> Some InvalidLookupAttributesException
  | "InvalidMaxResultsException" -> Some InvalidMaxResultsException
  | "InvalidNextTokenException" -> Some InvalidNextTokenException
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterCombinationException" -> Some InvalidParameterCombinationException
  | "InvalidParameterException" -> Some InvalidParameterException
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidQueryStatementException" -> Some InvalidQueryStatementException
  | "InvalidQueryStatusException" -> Some InvalidQueryStatusException
  | "InvalidS3BucketNameException" -> Some InvalidS3BucketNameException
  | "InvalidS3PrefixException" -> Some InvalidS3PrefixException
  | "InvalidSnsTopicNameException" -> Some InvalidSnsTopicNameException
  | "InvalidSourceException" -> Some InvalidSourceException
  | "InvalidTagParameterException" -> Some InvalidTagParameterException
  | "InvalidTimeRangeException" -> Some InvalidTimeRangeException
  | "InvalidTokenException" -> Some InvalidTokenException
  | "InvalidTrailNameException" -> Some InvalidTrailNameException
  | "KmsException" -> Some KmsException
  | "KmsKeyDisabledException" -> Some KmsKeyDisabledException
  | "KmsKeyNotFoundException" -> Some KmsKeyNotFoundException
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MaxConcurrentQueriesException" -> Some MaxConcurrentQueriesException
  | "MaximumNumberOfTrailsExceededException" ->
      Some MaximumNumberOfTrailsExceededException
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NoManagementAccountSLRExistsException" -> Some NoManagementAccountSLRExistsException
  | "NotOrganizationManagementAccountException" ->
      Some NotOrganizationManagementAccountException
  | "NotOrganizationMasterAccountException" -> Some NotOrganizationMasterAccountException
  | "OperationNotPermittedException" -> Some OperationNotPermittedException
  | "OptInRequired" -> Some OptInRequired
  | "OrganizationNotInAllFeaturesModeException" ->
      Some OrganizationNotInAllFeaturesModeException
  | "OrganizationsNotInUseException" -> Some OrganizationsNotInUseException
  | "PendingVerification" -> Some PendingVerification
  | "QueryIdNotFoundException" -> Some QueryIdNotFoundException
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ResourceARNNotValidException" -> Some ResourceARNNotValidException
  | "ResourceNotFoundException" -> Some ResourceNotFoundException
  | "ResourcePolicyNotFoundException" -> Some ResourcePolicyNotFoundException
  | "ResourcePolicyNotValidException" -> Some ResourcePolicyNotValidException
  | "ResourceTypeNotSupportedException" -> Some ResourceTypeNotSupportedException
  | "S3BucketDoesNotExistException" -> Some S3BucketDoesNotExistException
  | "ServiceQuotaExceededException" -> Some ServiceQuotaExceededException
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "TagsLimitExceededException" -> Some TagsLimitExceededException
  | "Throttling" -> Some Throttling
  | "ThrottlingException" -> Some ThrottlingException
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
