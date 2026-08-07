type t =
  | AuthFailure
  | Blocked
  | CidrBlockInUseException
  | CidrCollectionAlreadyExistsException
  | CidrCollectionInUseException
  | CidrCollectionVersionMismatchException
  | ConcurrentModification
  | ConflictingDomainExists
  | ConflictingTypes
  | DNSSECNotFound
  | DelegationSetAlreadyCreated
  | DelegationSetAlreadyReusable
  | DelegationSetInUse
  | DelegationSetNotAvailable
  | DelegationSetNotReusable
  | DryRunOperation
  | HealthCheckAlreadyExists
  | HealthCheckInUse
  | HealthCheckVersionMismatch
  | HostedZoneAlreadyExists
  | HostedZoneNotEmpty
  | HostedZoneNotFound
  | HostedZoneNotPrivate
  | HostedZonePartiallyDelegated
  | IdempotentParameterMismatch
  | IncompatibleVersion
  | IncompleteSignature
  | InsufficientCloudWatchLogsResourcePolicy
  | InternalFailure
  | InvalidAction
  | InvalidArgument
  | InvalidChangeBatch
  | InvalidClientTokenId
  | InvalidDomainName
  | InvalidInput
  | InvalidKMSArn
  | InvalidKeySigningKeyName
  | InvalidKeySigningKeyStatus
  | InvalidPaginationToken
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidSigningStatus
  | InvalidTrafficPolicyDocument
  | InvalidVPCId
  | KeySigningKeyAlreadyExists
  | KeySigningKeyInParentDSRecord
  | KeySigningKeyInUse
  | KeySigningKeyWithActiveStatusNotFound
  | LastVPCAssociation
  | LimitsExceeded
  | MalformedQueryString
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | NoSuchChange
  | NoSuchCidrCollectionException
  | NoSuchCidrLocationException
  | NoSuchCloudWatchLogsLogGroup
  | NoSuchDelegationSet
  | NoSuchGeoLocation
  | NoSuchHealthCheck
  | NoSuchHostedZone
  | NoSuchKeySigningKey
  | NoSuchQueryLoggingConfig
  | NoSuchTrafficPolicy
  | NoSuchTrafficPolicyInstance
  | NotAuthorizedException
  | OptInRequired
  | PendingVerification
  | PriorRequestNotComplete
  | PublicZoneVPCAssociation
  | QueryLoggingConfigAlreadyExists
  | RequestExpired
  | RequestLimitExceeded
  | ServiceUnavailable
  | Throttling
  | ThrottlingException
  | TooManyHealthChecks
  | TooManyHostedZones
  | TooManyKeySigningKeys
  | TooManyTrafficPolicies
  | TooManyTrafficPolicyInstances
  | TooManyTrafficPolicyVersionsForCurrentPolicy
  | TooManyVPCAssociationAuthorizations
  | TrafficPolicyAlreadyExists
  | TrafficPolicyInUse
  | TrafficPolicyInstanceAlreadyExists
  | UnauthorizedOperation
  | UnknownParameter
  | UnsupportedProtocol
  | VPCAssociationAuthorizationNotFound
  | VPCAssociationNotFound
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
  | CidrBlockInUseException -> Some 400
  | CidrCollectionAlreadyExistsException -> None
  | CidrCollectionInUseException -> Some 400
  | CidrCollectionVersionMismatchException -> Some 409
  | ConcurrentModification -> Some 400
  | ConflictingDomainExists -> None
  | ConflictingTypes -> Some 400
  | DNSSECNotFound -> Some 400
  | DelegationSetAlreadyCreated -> None
  | DelegationSetAlreadyReusable -> None
  | DelegationSetInUse -> None
  | DelegationSetNotAvailable -> None
  | DelegationSetNotReusable -> None
  | DryRunOperation -> None
  | HealthCheckAlreadyExists -> Some 409
  | HealthCheckInUse -> Some 400
  | HealthCheckVersionMismatch -> Some 409
  | HostedZoneAlreadyExists -> Some 409
  | HostedZoneNotEmpty -> Some 400
  | HostedZoneNotFound -> None
  | HostedZoneNotPrivate -> None
  | HostedZonePartiallyDelegated -> None
  | IdempotentParameterMismatch -> None
  | IncompatibleVersion -> Some 400
  | IncompleteSignature -> Some 400
  | InsufficientCloudWatchLogsResourcePolicy -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidArgument -> None
  | InvalidChangeBatch -> None
  | InvalidClientTokenId -> Some 403
  | InvalidDomainName -> Some 400
  | InvalidInput -> Some 400
  | InvalidKMSArn -> None
  | InvalidKeySigningKeyName -> Some 400
  | InvalidKeySigningKeyStatus -> Some 400
  | InvalidPaginationToken -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidSigningStatus -> None
  | InvalidTrafficPolicyDocument -> Some 400
  | InvalidVPCId -> Some 400
  | KeySigningKeyAlreadyExists -> Some 409
  | KeySigningKeyInParentDSRecord -> Some 400
  | KeySigningKeyInUse -> None
  | KeySigningKeyWithActiveStatusNotFound -> None
  | LastVPCAssociation -> Some 400
  | LimitsExceeded -> None
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NoSuchChange -> Some 404
  | NoSuchCidrCollectionException -> Some 404
  | NoSuchCidrLocationException -> Some 404
  | NoSuchCloudWatchLogsLogGroup -> Some 404
  | NoSuchDelegationSet -> None
  | NoSuchGeoLocation -> Some 404
  | NoSuchHealthCheck -> Some 404
  | NoSuchHostedZone -> Some 404
  | NoSuchKeySigningKey -> Some 404
  | NoSuchQueryLoggingConfig -> Some 404
  | NoSuchTrafficPolicy -> Some 404
  | NoSuchTrafficPolicyInstance -> Some 404
  | NotAuthorizedException -> Some 401
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | PriorRequestNotComplete -> Some 400
  | PublicZoneVPCAssociation -> Some 400
  | QueryLoggingConfigAlreadyExists -> Some 409
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | ThrottlingException -> Some 400
  | TooManyHealthChecks -> None
  | TooManyHostedZones -> Some 400
  | TooManyKeySigningKeys -> None
  | TooManyTrafficPolicies -> Some 400
  | TooManyTrafficPolicyInstances -> Some 400
  | TooManyTrafficPolicyVersionsForCurrentPolicy -> Some 400
  | TooManyVPCAssociationAuthorizations -> Some 400
  | TrafficPolicyAlreadyExists -> Some 409
  | TrafficPolicyInUse -> Some 400
  | TrafficPolicyInstanceAlreadyExists -> Some 409
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | VPCAssociationAuthorizationNotFound -> Some 404
  | VPCAssociationNotFound -> Some 404
  | ValidationError -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | CidrBlockInUseException -> "CidrBlockInUseException"
  | CidrCollectionAlreadyExistsException -> "CidrCollectionAlreadyExistsException"
  | CidrCollectionInUseException -> "CidrCollectionInUseException"
  | CidrCollectionVersionMismatchException -> "CidrCollectionVersionMismatchException"
  | ConcurrentModification -> "ConcurrentModification"
  | ConflictingDomainExists -> "ConflictingDomainExists"
  | ConflictingTypes -> "ConflictingTypes"
  | DNSSECNotFound -> "DNSSECNotFound"
  | DelegationSetAlreadyCreated -> "DelegationSetAlreadyCreated"
  | DelegationSetAlreadyReusable -> "DelegationSetAlreadyReusable"
  | DelegationSetInUse -> "DelegationSetInUse"
  | DelegationSetNotAvailable -> "DelegationSetNotAvailable"
  | DelegationSetNotReusable -> "DelegationSetNotReusable"
  | DryRunOperation -> "DryRunOperation"
  | HealthCheckAlreadyExists -> "HealthCheckAlreadyExists"
  | HealthCheckInUse -> "HealthCheckInUse"
  | HealthCheckVersionMismatch -> "HealthCheckVersionMismatch"
  | HostedZoneAlreadyExists -> "HostedZoneAlreadyExists"
  | HostedZoneNotEmpty -> "HostedZoneNotEmpty"
  | HostedZoneNotFound -> "HostedZoneNotFound"
  | HostedZoneNotPrivate -> "HostedZoneNotPrivate"
  | HostedZonePartiallyDelegated -> "HostedZonePartiallyDelegated"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompatibleVersion -> "IncompatibleVersion"
  | IncompleteSignature -> "IncompleteSignature"
  | InsufficientCloudWatchLogsResourcePolicy -> "InsufficientCloudWatchLogsResourcePolicy"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidArgument -> "InvalidArgument"
  | InvalidChangeBatch -> "InvalidChangeBatch"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidDomainName -> "InvalidDomainName"
  | InvalidInput -> "InvalidInput"
  | InvalidKMSArn -> "InvalidKMSArn"
  | InvalidKeySigningKeyName -> "InvalidKeySigningKeyName"
  | InvalidKeySigningKeyStatus -> "InvalidKeySigningKeyStatus"
  | InvalidPaginationToken -> "InvalidPaginationToken"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidSigningStatus -> "InvalidSigningStatus"
  | InvalidTrafficPolicyDocument -> "InvalidTrafficPolicyDocument"
  | InvalidVPCId -> "InvalidVPCId"
  | KeySigningKeyAlreadyExists -> "KeySigningKeyAlreadyExists"
  | KeySigningKeyInParentDSRecord -> "KeySigningKeyInParentDSRecord"
  | KeySigningKeyInUse -> "KeySigningKeyInUse"
  | KeySigningKeyWithActiveStatusNotFound -> "KeySigningKeyWithActiveStatusNotFound"
  | LastVPCAssociation -> "LastVPCAssociation"
  | LimitsExceeded -> "LimitsExceeded"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NoSuchChange -> "NoSuchChange"
  | NoSuchCidrCollectionException -> "NoSuchCidrCollectionException"
  | NoSuchCidrLocationException -> "NoSuchCidrLocationException"
  | NoSuchCloudWatchLogsLogGroup -> "NoSuchCloudWatchLogsLogGroup"
  | NoSuchDelegationSet -> "NoSuchDelegationSet"
  | NoSuchGeoLocation -> "NoSuchGeoLocation"
  | NoSuchHealthCheck -> "NoSuchHealthCheck"
  | NoSuchHostedZone -> "NoSuchHostedZone"
  | NoSuchKeySigningKey -> "NoSuchKeySigningKey"
  | NoSuchQueryLoggingConfig -> "NoSuchQueryLoggingConfig"
  | NoSuchTrafficPolicy -> "NoSuchTrafficPolicy"
  | NoSuchTrafficPolicyInstance -> "NoSuchTrafficPolicyInstance"
  | NotAuthorizedException -> "NotAuthorizedException"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | PriorRequestNotComplete -> "PriorRequestNotComplete"
  | PublicZoneVPCAssociation -> "PublicZoneVPCAssociation"
  | QueryLoggingConfigAlreadyExists -> "QueryLoggingConfigAlreadyExists"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | ThrottlingException -> "ThrottlingException"
  | TooManyHealthChecks -> "TooManyHealthChecks"
  | TooManyHostedZones -> "TooManyHostedZones"
  | TooManyKeySigningKeys -> "TooManyKeySigningKeys"
  | TooManyTrafficPolicies -> "TooManyTrafficPolicies"
  | TooManyTrafficPolicyInstances -> "TooManyTrafficPolicyInstances"
  | TooManyTrafficPolicyVersionsForCurrentPolicy ->
      "TooManyTrafficPolicyVersionsForCurrentPolicy"
  | TooManyVPCAssociationAuthorizations -> "TooManyVPCAssociationAuthorizations"
  | TrafficPolicyAlreadyExists -> "TrafficPolicyAlreadyExists"
  | TrafficPolicyInUse -> "TrafficPolicyInUse"
  | TrafficPolicyInstanceAlreadyExists -> "TrafficPolicyInstanceAlreadyExists"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | VPCAssociationAuthorizationNotFound -> "VPCAssociationAuthorizationNotFound"
  | VPCAssociationNotFound -> "VPCAssociationNotFound"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CidrBlockInUseException" -> Some CidrBlockInUseException
  | "CidrCollectionAlreadyExistsException" -> Some CidrCollectionAlreadyExistsException
  | "CidrCollectionInUseException" -> Some CidrCollectionInUseException
  | "CidrCollectionVersionMismatchException" ->
      Some CidrCollectionVersionMismatchException
  | "ConcurrentModification" -> Some ConcurrentModification
  | "ConflictingDomainExists" -> Some ConflictingDomainExists
  | "ConflictingTypes" -> Some ConflictingTypes
  | "DNSSECNotFound" -> Some DNSSECNotFound
  | "DelegationSetAlreadyCreated" -> Some DelegationSetAlreadyCreated
  | "DelegationSetAlreadyReusable" -> Some DelegationSetAlreadyReusable
  | "DelegationSetInUse" -> Some DelegationSetInUse
  | "DelegationSetNotAvailable" -> Some DelegationSetNotAvailable
  | "DelegationSetNotReusable" -> Some DelegationSetNotReusable
  | "DryRunOperation" -> Some DryRunOperation
  | "HealthCheckAlreadyExists" -> Some HealthCheckAlreadyExists
  | "HealthCheckInUse" -> Some HealthCheckInUse
  | "HealthCheckVersionMismatch" -> Some HealthCheckVersionMismatch
  | "HostedZoneAlreadyExists" -> Some HostedZoneAlreadyExists
  | "HostedZoneNotEmpty" -> Some HostedZoneNotEmpty
  | "HostedZoneNotFound" -> Some HostedZoneNotFound
  | "HostedZoneNotPrivate" -> Some HostedZoneNotPrivate
  | "HostedZonePartiallyDelegated" -> Some HostedZonePartiallyDelegated
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompatibleVersion" -> Some IncompatibleVersion
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InsufficientCloudWatchLogsResourcePolicy" ->
      Some InsufficientCloudWatchLogsResourcePolicy
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidArgument" -> Some InvalidArgument
  | "InvalidChangeBatch" -> Some InvalidChangeBatch
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidDomainName" -> Some InvalidDomainName
  | "InvalidInput" -> Some InvalidInput
  | "InvalidKMSArn" -> Some InvalidKMSArn
  | "InvalidKeySigningKeyName" -> Some InvalidKeySigningKeyName
  | "InvalidKeySigningKeyStatus" -> Some InvalidKeySigningKeyStatus
  | "InvalidPaginationToken" -> Some InvalidPaginationToken
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidSigningStatus" -> Some InvalidSigningStatus
  | "InvalidTrafficPolicyDocument" -> Some InvalidTrafficPolicyDocument
  | "InvalidVPCId" -> Some InvalidVPCId
  | "KeySigningKeyAlreadyExists" -> Some KeySigningKeyAlreadyExists
  | "KeySigningKeyInParentDSRecord" -> Some KeySigningKeyInParentDSRecord
  | "KeySigningKeyInUse" -> Some KeySigningKeyInUse
  | "KeySigningKeyWithActiveStatusNotFound" -> Some KeySigningKeyWithActiveStatusNotFound
  | "LastVPCAssociation" -> Some LastVPCAssociation
  | "LimitsExceeded" -> Some LimitsExceeded
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NoSuchChange" -> Some NoSuchChange
  | "NoSuchCidrCollectionException" -> Some NoSuchCidrCollectionException
  | "NoSuchCidrLocationException" -> Some NoSuchCidrLocationException
  | "NoSuchCloudWatchLogsLogGroup" -> Some NoSuchCloudWatchLogsLogGroup
  | "NoSuchDelegationSet" -> Some NoSuchDelegationSet
  | "NoSuchGeoLocation" -> Some NoSuchGeoLocation
  | "NoSuchHealthCheck" -> Some NoSuchHealthCheck
  | "NoSuchHostedZone" -> Some NoSuchHostedZone
  | "NoSuchKeySigningKey" -> Some NoSuchKeySigningKey
  | "NoSuchQueryLoggingConfig" -> Some NoSuchQueryLoggingConfig
  | "NoSuchTrafficPolicy" -> Some NoSuchTrafficPolicy
  | "NoSuchTrafficPolicyInstance" -> Some NoSuchTrafficPolicyInstance
  | "NotAuthorizedException" -> Some NotAuthorizedException
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "PriorRequestNotComplete" -> Some PriorRequestNotComplete
  | "PublicZoneVPCAssociation" -> Some PublicZoneVPCAssociation
  | "QueryLoggingConfigAlreadyExists" -> Some QueryLoggingConfigAlreadyExists
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "ThrottlingException" -> Some ThrottlingException
  | "TooManyHealthChecks" -> Some TooManyHealthChecks
  | "TooManyHostedZones" -> Some TooManyHostedZones
  | "TooManyKeySigningKeys" -> Some TooManyKeySigningKeys
  | "TooManyTrafficPolicies" -> Some TooManyTrafficPolicies
  | "TooManyTrafficPolicyInstances" -> Some TooManyTrafficPolicyInstances
  | "TooManyTrafficPolicyVersionsForCurrentPolicy" ->
      Some TooManyTrafficPolicyVersionsForCurrentPolicy
  | "TooManyVPCAssociationAuthorizations" -> Some TooManyVPCAssociationAuthorizations
  | "TrafficPolicyAlreadyExists" -> Some TrafficPolicyAlreadyExists
  | "TrafficPolicyInUse" -> Some TrafficPolicyInUse
  | "TrafficPolicyInstanceAlreadyExists" -> Some TrafficPolicyInstanceAlreadyExists
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "VPCAssociationAuthorizationNotFound" -> Some VPCAssociationAuthorizationNotFound
  | "VPCAssociationNotFound" -> Some VPCAssociationNotFound
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
