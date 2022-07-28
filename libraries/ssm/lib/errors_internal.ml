type t =
  | AlreadyExistsException
  | AssociatedInstances
  | AssociationAlreadyExists
  | AssociationDoesNotExist
  | AssociationExecutionDoesNotExist
  | AssociationLimitExceeded
  | AssociationVersionLimitExceeded
  | AuthFailure
  | AutomationDefinitionNotFoundException
  | AutomationDefinitionVersionNotFoundException
  | AutomationExecutionLimitExceededException
  | AutomationExecutionNotFoundException
  | AutomationStepNotFoundException
  | Blocked
  | ComplianceTypeCountLimitExceededException
  | CustomSchemaCountLimitExceededException
  | DocumentAlreadyExists
  | DocumentLimitExceeded
  | DocumentPermissionLimit
  | DocumentVersionLimitExceeded
  | DoesNotExistException
  | DryRunOperation
  | DuplicateDocumentContent
  | DuplicateDocumentVersionName
  | DuplicateInstanceId
  | FeatureNotAvailableException
  | HierarchyLevelLimitExceededException
  | HierarchyTypeMismatchException
  | IdempotentParameterMismatch
  | IncompatiblePolicyException
  | IncompleteSignature
  | InternalFailure
  | InternalServerError
  | InvalidAction
  | InvalidActivation
  | InvalidActivationId
  | InvalidAggregatorException
  | InvalidAllowedPatternException
  | InvalidAssociation
  | InvalidAssociationVersion
  | InvalidAutomationExecutionParametersException
  | InvalidAutomationSignalException
  | InvalidAutomationStatusUpdateException
  | InvalidClientTokenId
  | InvalidCommandId
  | InvalidDeleteInventoryParametersException
  | InvalidDeletionIdException
  | InvalidDocument
  | InvalidDocumentContent
  | InvalidDocumentOperation
  | InvalidDocumentSchemaVersion
  | InvalidDocumentType
  | InvalidDocumentVersion
  | InvalidFilter
  | InvalidFilterKey
  | InvalidFilterOption
  | InvalidFilterValue
  | InvalidInstanceId
  | InvalidInstanceInformationFilterValue
  | InvalidInventoryGroupException
  | InvalidInventoryItemContextException
  | InvalidInventoryRequestException
  | InvalidItemContentException
  | InvalidKeyId
  | InvalidNextToken
  | InvalidNotificationConfig
  | InvalidOptionException
  | InvalidOutputFolder
  | InvalidOutputLocation
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidParameters
  | InvalidPermissionType
  | InvalidPluginName
  | InvalidPolicyAttributeException
  | InvalidPolicyTypeException
  | InvalidQueryParameter
  | InvalidResourceId
  | InvalidResourceType
  | InvalidResultAttributeException
  | InvalidRole
  | InvalidSchedule
  | InvalidTarget
  | InvalidTypeNameException
  | InvalidUpdate
  | InvocationDoesNotExist
  | ItemContentMismatchException
  | ItemSizeLimitExceededException
  | MalformedQueryString
  | MaxDocumentSizeExceeded
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | OpsItemAlreadyExistsException
  | OpsItemInvalidParameterException
  | OpsItemLimitExceededException
  | OpsItemNotFoundException
  | OptInRequired
  | ParameterAlreadyExists
  | ParameterLimitExceeded
  | ParameterMaxVersionLimitExceeded
  | ParameterNotFound
  | ParameterPatternMismatchException
  | ParameterVersionLabelLimitExceeded
  | ParameterVersionNotFound
  | PendingVerification
  | PoliciesLimitExceededException
  | RequestExpired
  | RequestLimitExceeded
  | ResourceDataSyncAlreadyExistsException
  | ResourceDataSyncConflictException
  | ResourceDataSyncCountExceededException
  | ResourceDataSyncInvalidConfigurationException
  | ResourceDataSyncNotFoundException
  | ResourceInUseException
  | ResourceLimitExceededException
  | ServiceSettingNotFound
  | ServiceUnavailable
  | StatusUnchanged
  | SubTypeCountLimitExceededException
  | TargetInUseException
  | TargetNotConnected
  | Throttling
  | TooManyTagsError
  | TooManyUpdates
  | TotalSizeLimitExceededException
  | UnauthorizedOperation
  | UnknownParameter
  | UnsupportedCalendarException
  | UnsupportedFeatureRequiredException
  | UnsupportedInventoryItemContextException
  | UnsupportedInventorySchemaVersionException
  | UnsupportedOperatingSystem
  | UnsupportedParameterType
  | UnsupportedPlatformType
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
  | AlreadyExistsException -> None
  | AssociatedInstances -> None
  | AssociationAlreadyExists -> None
  | AssociationDoesNotExist -> None
  | AssociationExecutionDoesNotExist -> None
  | AssociationLimitExceeded -> None
  | AssociationVersionLimitExceeded -> None
  | AuthFailure -> None
  | AutomationDefinitionNotFoundException -> None
  | AutomationDefinitionVersionNotFoundException -> None
  | AutomationExecutionLimitExceededException -> None
  | AutomationExecutionNotFoundException -> None
  | AutomationStepNotFoundException -> None
  | Blocked -> None
  | ComplianceTypeCountLimitExceededException -> None
  | CustomSchemaCountLimitExceededException -> None
  | DocumentAlreadyExists -> None
  | DocumentLimitExceeded -> None
  | DocumentPermissionLimit -> None
  | DocumentVersionLimitExceeded -> None
  | DoesNotExistException -> None
  | DryRunOperation -> None
  | DuplicateDocumentContent -> None
  | DuplicateDocumentVersionName -> None
  | DuplicateInstanceId -> None
  | FeatureNotAvailableException -> None
  | HierarchyLevelLimitExceededException -> None
  | HierarchyTypeMismatchException -> None
  | IdempotentParameterMismatch -> None
  | IncompatiblePolicyException -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InternalServerError -> None
  | InvalidAction -> Some 400
  | InvalidActivation -> None
  | InvalidActivationId -> None
  | InvalidAggregatorException -> None
  | InvalidAllowedPatternException -> None
  | InvalidAssociation -> None
  | InvalidAssociationVersion -> None
  | InvalidAutomationExecutionParametersException -> None
  | InvalidAutomationSignalException -> None
  | InvalidAutomationStatusUpdateException -> None
  | InvalidClientTokenId -> Some 403
  | InvalidCommandId -> None
  | InvalidDeleteInventoryParametersException -> None
  | InvalidDeletionIdException -> None
  | InvalidDocument -> None
  | InvalidDocumentContent -> None
  | InvalidDocumentOperation -> None
  | InvalidDocumentSchemaVersion -> None
  | InvalidDocumentType -> None
  | InvalidDocumentVersion -> None
  | InvalidFilter -> None
  | InvalidFilterKey -> None
  | InvalidFilterOption -> None
  | InvalidFilterValue -> None
  | InvalidInstanceId -> None
  | InvalidInstanceInformationFilterValue -> None
  | InvalidInventoryGroupException -> None
  | InvalidInventoryItemContextException -> None
  | InvalidInventoryRequestException -> None
  | InvalidItemContentException -> None
  | InvalidKeyId -> None
  | InvalidNextToken -> None
  | InvalidNotificationConfig -> None
  | InvalidOptionException -> None
  | InvalidOutputFolder -> None
  | InvalidOutputLocation -> None
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidParameters -> None
  | InvalidPermissionType -> None
  | InvalidPluginName -> None
  | InvalidPolicyAttributeException -> None
  | InvalidPolicyTypeException -> None
  | InvalidQueryParameter -> Some 400
  | InvalidResourceId -> None
  | InvalidResourceType -> None
  | InvalidResultAttributeException -> None
  | InvalidRole -> None
  | InvalidSchedule -> None
  | InvalidTarget -> None
  | InvalidTypeNameException -> None
  | InvalidUpdate -> None
  | InvocationDoesNotExist -> None
  | ItemContentMismatchException -> None
  | ItemSizeLimitExceededException -> None
  | MalformedQueryString -> Some 404
  | MaxDocumentSizeExceeded -> None
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OpsItemAlreadyExistsException -> None
  | OpsItemInvalidParameterException -> None
  | OpsItemLimitExceededException -> None
  | OpsItemNotFoundException -> None
  | OptInRequired -> Some 403
  | ParameterAlreadyExists -> None
  | ParameterLimitExceeded -> None
  | ParameterMaxVersionLimitExceeded -> None
  | ParameterNotFound -> None
  | ParameterPatternMismatchException -> None
  | ParameterVersionLabelLimitExceeded -> None
  | ParameterVersionNotFound -> None
  | PendingVerification -> None
  | PoliciesLimitExceededException -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ResourceDataSyncAlreadyExistsException -> None
  | ResourceDataSyncConflictException -> None
  | ResourceDataSyncCountExceededException -> None
  | ResourceDataSyncInvalidConfigurationException -> None
  | ResourceDataSyncNotFoundException -> None
  | ResourceInUseException -> None
  | ResourceLimitExceededException -> None
  | ServiceSettingNotFound -> None
  | ServiceUnavailable -> Some 503
  | StatusUnchanged -> None
  | SubTypeCountLimitExceededException -> None
  | TargetInUseException -> None
  | TargetNotConnected -> None
  | Throttling -> Some 400
  | TooManyTagsError -> None
  | TooManyUpdates -> None
  | TotalSizeLimitExceededException -> None
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedCalendarException -> None
  | UnsupportedFeatureRequiredException -> None
  | UnsupportedInventoryItemContextException -> None
  | UnsupportedInventorySchemaVersionException -> None
  | UnsupportedOperatingSystem -> None
  | UnsupportedParameterType -> None
  | UnsupportedPlatformType -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AlreadyExistsException -> "AlreadyExistsException"
  | AssociatedInstances -> "AssociatedInstances"
  | AssociationAlreadyExists -> "AssociationAlreadyExists"
  | AssociationDoesNotExist -> "AssociationDoesNotExist"
  | AssociationExecutionDoesNotExist -> "AssociationExecutionDoesNotExist"
  | AssociationLimitExceeded -> "AssociationLimitExceeded"
  | AssociationVersionLimitExceeded -> "AssociationVersionLimitExceeded"
  | AuthFailure -> "AuthFailure"
  | AutomationDefinitionNotFoundException -> "AutomationDefinitionNotFoundException"
  | AutomationDefinitionVersionNotFoundException ->
      "AutomationDefinitionVersionNotFoundException"
  | AutomationExecutionLimitExceededException ->
      "AutomationExecutionLimitExceededException"
  | AutomationExecutionNotFoundException -> "AutomationExecutionNotFoundException"
  | AutomationStepNotFoundException -> "AutomationStepNotFoundException"
  | Blocked -> "Blocked"
  | ComplianceTypeCountLimitExceededException ->
      "ComplianceTypeCountLimitExceededException"
  | CustomSchemaCountLimitExceededException -> "CustomSchemaCountLimitExceededException"
  | DocumentAlreadyExists -> "DocumentAlreadyExists"
  | DocumentLimitExceeded -> "DocumentLimitExceeded"
  | DocumentPermissionLimit -> "DocumentPermissionLimit"
  | DocumentVersionLimitExceeded -> "DocumentVersionLimitExceeded"
  | DoesNotExistException -> "DoesNotExistException"
  | DryRunOperation -> "DryRunOperation"
  | DuplicateDocumentContent -> "DuplicateDocumentContent"
  | DuplicateDocumentVersionName -> "DuplicateDocumentVersionName"
  | DuplicateInstanceId -> "DuplicateInstanceId"
  | FeatureNotAvailableException -> "FeatureNotAvailableException"
  | HierarchyLevelLimitExceededException -> "HierarchyLevelLimitExceededException"
  | HierarchyTypeMismatchException -> "HierarchyTypeMismatchException"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompatiblePolicyException -> "IncompatiblePolicyException"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InternalServerError -> "InternalServerError"
  | InvalidAction -> "InvalidAction"
  | InvalidActivation -> "InvalidActivation"
  | InvalidActivationId -> "InvalidActivationId"
  | InvalidAggregatorException -> "InvalidAggregatorException"
  | InvalidAllowedPatternException -> "InvalidAllowedPatternException"
  | InvalidAssociation -> "InvalidAssociation"
  | InvalidAssociationVersion -> "InvalidAssociationVersion"
  | InvalidAutomationExecutionParametersException ->
      "InvalidAutomationExecutionParametersException"
  | InvalidAutomationSignalException -> "InvalidAutomationSignalException"
  | InvalidAutomationStatusUpdateException -> "InvalidAutomationStatusUpdateException"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidCommandId -> "InvalidCommandId"
  | InvalidDeleteInventoryParametersException ->
      "InvalidDeleteInventoryParametersException"
  | InvalidDeletionIdException -> "InvalidDeletionIdException"
  | InvalidDocument -> "InvalidDocument"
  | InvalidDocumentContent -> "InvalidDocumentContent"
  | InvalidDocumentOperation -> "InvalidDocumentOperation"
  | InvalidDocumentSchemaVersion -> "InvalidDocumentSchemaVersion"
  | InvalidDocumentType -> "InvalidDocumentType"
  | InvalidDocumentVersion -> "InvalidDocumentVersion"
  | InvalidFilter -> "InvalidFilter"
  | InvalidFilterKey -> "InvalidFilterKey"
  | InvalidFilterOption -> "InvalidFilterOption"
  | InvalidFilterValue -> "InvalidFilterValue"
  | InvalidInstanceId -> "InvalidInstanceId"
  | InvalidInstanceInformationFilterValue -> "InvalidInstanceInformationFilterValue"
  | InvalidInventoryGroupException -> "InvalidInventoryGroupException"
  | InvalidInventoryItemContextException -> "InvalidInventoryItemContextException"
  | InvalidInventoryRequestException -> "InvalidInventoryRequestException"
  | InvalidItemContentException -> "InvalidItemContentException"
  | InvalidKeyId -> "InvalidKeyId"
  | InvalidNextToken -> "InvalidNextToken"
  | InvalidNotificationConfig -> "InvalidNotificationConfig"
  | InvalidOptionException -> "InvalidOptionException"
  | InvalidOutputFolder -> "InvalidOutputFolder"
  | InvalidOutputLocation -> "InvalidOutputLocation"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidParameters -> "InvalidParameters"
  | InvalidPermissionType -> "InvalidPermissionType"
  | InvalidPluginName -> "InvalidPluginName"
  | InvalidPolicyAttributeException -> "InvalidPolicyAttributeException"
  | InvalidPolicyTypeException -> "InvalidPolicyTypeException"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidResourceId -> "InvalidResourceId"
  | InvalidResourceType -> "InvalidResourceType"
  | InvalidResultAttributeException -> "InvalidResultAttributeException"
  | InvalidRole -> "InvalidRole"
  | InvalidSchedule -> "InvalidSchedule"
  | InvalidTarget -> "InvalidTarget"
  | InvalidTypeNameException -> "InvalidTypeNameException"
  | InvalidUpdate -> "InvalidUpdate"
  | InvocationDoesNotExist -> "InvocationDoesNotExist"
  | ItemContentMismatchException -> "ItemContentMismatchException"
  | ItemSizeLimitExceededException -> "ItemSizeLimitExceededException"
  | MalformedQueryString -> "MalformedQueryString"
  | MaxDocumentSizeExceeded -> "MaxDocumentSizeExceeded"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OpsItemAlreadyExistsException -> "OpsItemAlreadyExistsException"
  | OpsItemInvalidParameterException -> "OpsItemInvalidParameterException"
  | OpsItemLimitExceededException -> "OpsItemLimitExceededException"
  | OpsItemNotFoundException -> "OpsItemNotFoundException"
  | OptInRequired -> "OptInRequired"
  | ParameterAlreadyExists -> "ParameterAlreadyExists"
  | ParameterLimitExceeded -> "ParameterLimitExceeded"
  | ParameterMaxVersionLimitExceeded -> "ParameterMaxVersionLimitExceeded"
  | ParameterNotFound -> "ParameterNotFound"
  | ParameterPatternMismatchException -> "ParameterPatternMismatchException"
  | ParameterVersionLabelLimitExceeded -> "ParameterVersionLabelLimitExceeded"
  | ParameterVersionNotFound -> "ParameterVersionNotFound"
  | PendingVerification -> "PendingVerification"
  | PoliciesLimitExceededException -> "PoliciesLimitExceededException"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ResourceDataSyncAlreadyExistsException -> "ResourceDataSyncAlreadyExistsException"
  | ResourceDataSyncConflictException -> "ResourceDataSyncConflictException"
  | ResourceDataSyncCountExceededException -> "ResourceDataSyncCountExceededException"
  | ResourceDataSyncInvalidConfigurationException ->
      "ResourceDataSyncInvalidConfigurationException"
  | ResourceDataSyncNotFoundException -> "ResourceDataSyncNotFoundException"
  | ResourceInUseException -> "ResourceInUseException"
  | ResourceLimitExceededException -> "ResourceLimitExceededException"
  | ServiceSettingNotFound -> "ServiceSettingNotFound"
  | ServiceUnavailable -> "ServiceUnavailable"
  | StatusUnchanged -> "StatusUnchanged"
  | SubTypeCountLimitExceededException -> "SubTypeCountLimitExceededException"
  | TargetInUseException -> "TargetInUseException"
  | TargetNotConnected -> "TargetNotConnected"
  | Throttling -> "Throttling"
  | TooManyTagsError -> "TooManyTagsError"
  | TooManyUpdates -> "TooManyUpdates"
  | TotalSizeLimitExceededException -> "TotalSizeLimitExceededException"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedCalendarException -> "UnsupportedCalendarException"
  | UnsupportedFeatureRequiredException -> "UnsupportedFeatureRequiredException"
  | UnsupportedInventoryItemContextException -> "UnsupportedInventoryItemContextException"
  | UnsupportedInventorySchemaVersionException ->
      "UnsupportedInventorySchemaVersionException"
  | UnsupportedOperatingSystem -> "UnsupportedOperatingSystem"
  | UnsupportedParameterType -> "UnsupportedParameterType"
  | UnsupportedPlatformType -> "UnsupportedPlatformType"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AlreadyExistsException" -> Some AlreadyExistsException
  | "AssociatedInstances" -> Some AssociatedInstances
  | "AssociationAlreadyExists" -> Some AssociationAlreadyExists
  | "AssociationDoesNotExist" -> Some AssociationDoesNotExist
  | "AssociationExecutionDoesNotExist" -> Some AssociationExecutionDoesNotExist
  | "AssociationLimitExceeded" -> Some AssociationLimitExceeded
  | "AssociationVersionLimitExceeded" -> Some AssociationVersionLimitExceeded
  | "AuthFailure" -> Some AuthFailure
  | "AutomationDefinitionNotFoundException" -> Some AutomationDefinitionNotFoundException
  | "AutomationDefinitionVersionNotFoundException" ->
      Some AutomationDefinitionVersionNotFoundException
  | "AutomationExecutionLimitExceededException" ->
      Some AutomationExecutionLimitExceededException
  | "AutomationExecutionNotFoundException" -> Some AutomationExecutionNotFoundException
  | "AutomationStepNotFoundException" -> Some AutomationStepNotFoundException
  | "Blocked" -> Some Blocked
  | "ComplianceTypeCountLimitExceededException" ->
      Some ComplianceTypeCountLimitExceededException
  | "CustomSchemaCountLimitExceededException" ->
      Some CustomSchemaCountLimitExceededException
  | "DocumentAlreadyExists" -> Some DocumentAlreadyExists
  | "DocumentLimitExceeded" -> Some DocumentLimitExceeded
  | "DocumentPermissionLimit" -> Some DocumentPermissionLimit
  | "DocumentVersionLimitExceeded" -> Some DocumentVersionLimitExceeded
  | "DoesNotExistException" -> Some DoesNotExistException
  | "DryRunOperation" -> Some DryRunOperation
  | "DuplicateDocumentContent" -> Some DuplicateDocumentContent
  | "DuplicateDocumentVersionName" -> Some DuplicateDocumentVersionName
  | "DuplicateInstanceId" -> Some DuplicateInstanceId
  | "FeatureNotAvailableException" -> Some FeatureNotAvailableException
  | "HierarchyLevelLimitExceededException" -> Some HierarchyLevelLimitExceededException
  | "HierarchyTypeMismatchException" -> Some HierarchyTypeMismatchException
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompatiblePolicyException" -> Some IncompatiblePolicyException
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InternalServerError" -> Some InternalServerError
  | "InvalidAction" -> Some InvalidAction
  | "InvalidActivation" -> Some InvalidActivation
  | "InvalidActivationId" -> Some InvalidActivationId
  | "InvalidAggregatorException" -> Some InvalidAggregatorException
  | "InvalidAllowedPatternException" -> Some InvalidAllowedPatternException
  | "InvalidAssociation" -> Some InvalidAssociation
  | "InvalidAssociationVersion" -> Some InvalidAssociationVersion
  | "InvalidAutomationExecutionParametersException" ->
      Some InvalidAutomationExecutionParametersException
  | "InvalidAutomationSignalException" -> Some InvalidAutomationSignalException
  | "InvalidAutomationStatusUpdateException" ->
      Some InvalidAutomationStatusUpdateException
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidCommandId" -> Some InvalidCommandId
  | "InvalidDeleteInventoryParametersException" ->
      Some InvalidDeleteInventoryParametersException
  | "InvalidDeletionIdException" -> Some InvalidDeletionIdException
  | "InvalidDocument" -> Some InvalidDocument
  | "InvalidDocumentContent" -> Some InvalidDocumentContent
  | "InvalidDocumentOperation" -> Some InvalidDocumentOperation
  | "InvalidDocumentSchemaVersion" -> Some InvalidDocumentSchemaVersion
  | "InvalidDocumentType" -> Some InvalidDocumentType
  | "InvalidDocumentVersion" -> Some InvalidDocumentVersion
  | "InvalidFilter" -> Some InvalidFilter
  | "InvalidFilterKey" -> Some InvalidFilterKey
  | "InvalidFilterOption" -> Some InvalidFilterOption
  | "InvalidFilterValue" -> Some InvalidFilterValue
  | "InvalidInstanceId" -> Some InvalidInstanceId
  | "InvalidInstanceInformationFilterValue" -> Some InvalidInstanceInformationFilterValue
  | "InvalidInventoryGroupException" -> Some InvalidInventoryGroupException
  | "InvalidInventoryItemContextException" -> Some InvalidInventoryItemContextException
  | "InvalidInventoryRequestException" -> Some InvalidInventoryRequestException
  | "InvalidItemContentException" -> Some InvalidItemContentException
  | "InvalidKeyId" -> Some InvalidKeyId
  | "InvalidNextToken" -> Some InvalidNextToken
  | "InvalidNotificationConfig" -> Some InvalidNotificationConfig
  | "InvalidOptionException" -> Some InvalidOptionException
  | "InvalidOutputFolder" -> Some InvalidOutputFolder
  | "InvalidOutputLocation" -> Some InvalidOutputLocation
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidParameters" -> Some InvalidParameters
  | "InvalidPermissionType" -> Some InvalidPermissionType
  | "InvalidPluginName" -> Some InvalidPluginName
  | "InvalidPolicyAttributeException" -> Some InvalidPolicyAttributeException
  | "InvalidPolicyTypeException" -> Some InvalidPolicyTypeException
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidResourceId" -> Some InvalidResourceId
  | "InvalidResourceType" -> Some InvalidResourceType
  | "InvalidResultAttributeException" -> Some InvalidResultAttributeException
  | "InvalidRole" -> Some InvalidRole
  | "InvalidSchedule" -> Some InvalidSchedule
  | "InvalidTarget" -> Some InvalidTarget
  | "InvalidTypeNameException" -> Some InvalidTypeNameException
  | "InvalidUpdate" -> Some InvalidUpdate
  | "InvocationDoesNotExist" -> Some InvocationDoesNotExist
  | "ItemContentMismatchException" -> Some ItemContentMismatchException
  | "ItemSizeLimitExceededException" -> Some ItemSizeLimitExceededException
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MaxDocumentSizeExceeded" -> Some MaxDocumentSizeExceeded
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OpsItemAlreadyExistsException" -> Some OpsItemAlreadyExistsException
  | "OpsItemInvalidParameterException" -> Some OpsItemInvalidParameterException
  | "OpsItemLimitExceededException" -> Some OpsItemLimitExceededException
  | "OpsItemNotFoundException" -> Some OpsItemNotFoundException
  | "OptInRequired" -> Some OptInRequired
  | "ParameterAlreadyExists" -> Some ParameterAlreadyExists
  | "ParameterLimitExceeded" -> Some ParameterLimitExceeded
  | "ParameterMaxVersionLimitExceeded" -> Some ParameterMaxVersionLimitExceeded
  | "ParameterNotFound" -> Some ParameterNotFound
  | "ParameterPatternMismatchException" -> Some ParameterPatternMismatchException
  | "ParameterVersionLabelLimitExceeded" -> Some ParameterVersionLabelLimitExceeded
  | "ParameterVersionNotFound" -> Some ParameterVersionNotFound
  | "PendingVerification" -> Some PendingVerification
  | "PoliciesLimitExceededException" -> Some PoliciesLimitExceededException
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ResourceDataSyncAlreadyExistsException" ->
      Some ResourceDataSyncAlreadyExistsException
  | "ResourceDataSyncConflictException" -> Some ResourceDataSyncConflictException
  | "ResourceDataSyncCountExceededException" ->
      Some ResourceDataSyncCountExceededException
  | "ResourceDataSyncInvalidConfigurationException" ->
      Some ResourceDataSyncInvalidConfigurationException
  | "ResourceDataSyncNotFoundException" -> Some ResourceDataSyncNotFoundException
  | "ResourceInUseException" -> Some ResourceInUseException
  | "ResourceLimitExceededException" -> Some ResourceLimitExceededException
  | "ServiceSettingNotFound" -> Some ServiceSettingNotFound
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "StatusUnchanged" -> Some StatusUnchanged
  | "SubTypeCountLimitExceededException" -> Some SubTypeCountLimitExceededException
  | "TargetInUseException" -> Some TargetInUseException
  | "TargetNotConnected" -> Some TargetNotConnected
  | "Throttling" -> Some Throttling
  | "TooManyTagsError" -> Some TooManyTagsError
  | "TooManyUpdates" -> Some TooManyUpdates
  | "TotalSizeLimitExceededException" -> Some TotalSizeLimitExceededException
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedCalendarException" -> Some UnsupportedCalendarException
  | "UnsupportedFeatureRequiredException" -> Some UnsupportedFeatureRequiredException
  | "UnsupportedInventoryItemContextException" ->
      Some UnsupportedInventoryItemContextException
  | "UnsupportedInventorySchemaVersionException" ->
      Some UnsupportedInventorySchemaVersionException
  | "UnsupportedOperatingSystem" -> Some UnsupportedOperatingSystem
  | "UnsupportedParameterType" -> Some UnsupportedParameterType
  | "UnsupportedPlatformType" -> Some UnsupportedPlatformType
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
