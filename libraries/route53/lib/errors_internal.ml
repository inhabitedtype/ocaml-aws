type t =
  | AuthFailure 
  | Blocked 
  | ConcurrentModification 
  | ConflictingDomainExists 
  | ConflictingTypes 
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
  | InvalidPaginationToken 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | InvalidTrafficPolicyDocument 
  | InvalidVPCId 
  | LastVPCAssociation 
  | LimitsExceeded 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | NoSuchChange 
  | NoSuchCloudWatchLogsLogGroup 
  | NoSuchDelegationSet 
  | NoSuchGeoLocation 
  | NoSuchHealthCheck 
  | NoSuchHostedZone 
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
  | ConcurrentModification -> Some 400
  | ConflictingDomainExists -> None
  | ConflictingTypes -> Some 400
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
  | InvalidPaginationToken -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidTrafficPolicyDocument -> Some 400
  | InvalidVPCId -> Some 400
  | LastVPCAssociation -> Some 400
  | LimitsExceeded -> None
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NoSuchChange -> Some 404
  | NoSuchCloudWatchLogsLogGroup -> Some 404
  | NoSuchDelegationSet -> None
  | NoSuchGeoLocation -> Some 404
  | NoSuchHealthCheck -> Some 404
  | NoSuchHostedZone -> Some 404
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
  | ConcurrentModification -> "ConcurrentModification"
  | ConflictingDomainExists -> "ConflictingDomainExists"
  | ConflictingTypes -> "ConflictingTypes"
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
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompatibleVersion -> "IncompatibleVersion"
  | IncompleteSignature -> "IncompleteSignature"
  | InsufficientCloudWatchLogsResourcePolicy ->
      "InsufficientCloudWatchLogsResourcePolicy"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidArgument -> "InvalidArgument"
  | InvalidChangeBatch -> "InvalidChangeBatch"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidDomainName -> "InvalidDomainName"
  | InvalidInput -> "InvalidInput"
  | InvalidPaginationToken -> "InvalidPaginationToken"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidTrafficPolicyDocument -> "InvalidTrafficPolicyDocument"
  | InvalidVPCId -> "InvalidVPCId"
  | LastVPCAssociation -> "LastVPCAssociation"
  | LimitsExceeded -> "LimitsExceeded"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NoSuchChange -> "NoSuchChange"
  | NoSuchCloudWatchLogsLogGroup -> "NoSuchCloudWatchLogsLogGroup"
  | NoSuchDelegationSet -> "NoSuchDelegationSet"
  | NoSuchGeoLocation -> "NoSuchGeoLocation"
  | NoSuchHealthCheck -> "NoSuchHealthCheck"
  | NoSuchHostedZone -> "NoSuchHostedZone"
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
  | TooManyTrafficPolicies -> "TooManyTrafficPolicies"
  | TooManyTrafficPolicyInstances -> "TooManyTrafficPolicyInstances"
  | TooManyTrafficPolicyVersionsForCurrentPolicy ->
      "TooManyTrafficPolicyVersionsForCurrentPolicy"
  | TooManyVPCAssociationAuthorizations ->
      "TooManyVPCAssociationAuthorizations"
  | TrafficPolicyAlreadyExists -> "TrafficPolicyAlreadyExists"
  | TrafficPolicyInUse -> "TrafficPolicyInUse"
  | TrafficPolicyInstanceAlreadyExists ->
      "TrafficPolicyInstanceAlreadyExists"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | VPCAssociationAuthorizationNotFound ->
      "VPCAssociationAuthorizationNotFound"
  | VPCAssociationNotFound -> "VPCAssociationNotFound"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "ConcurrentModification" -> Some ConcurrentModification
  | "ConflictingDomainExists" -> Some ConflictingDomainExists
  | "ConflictingTypes" -> Some ConflictingTypes
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
  | "InvalidPaginationToken" -> Some InvalidPaginationToken
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidTrafficPolicyDocument" -> Some InvalidTrafficPolicyDocument
  | "InvalidVPCId" -> Some InvalidVPCId
  | "LastVPCAssociation" -> Some LastVPCAssociation
  | "LimitsExceeded" -> Some LimitsExceeded
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NoSuchChange" -> Some NoSuchChange
  | "NoSuchCloudWatchLogsLogGroup" -> Some NoSuchCloudWatchLogsLogGroup
  | "NoSuchDelegationSet" -> Some NoSuchDelegationSet
  | "NoSuchGeoLocation" -> Some NoSuchGeoLocation
  | "NoSuchHealthCheck" -> Some NoSuchHealthCheck
  | "NoSuchHostedZone" -> Some NoSuchHostedZone
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
  | "TooManyTrafficPolicies" -> Some TooManyTrafficPolicies
  | "TooManyTrafficPolicyInstances" -> Some TooManyTrafficPolicyInstances
  | "TooManyTrafficPolicyVersionsForCurrentPolicy" ->
      Some TooManyTrafficPolicyVersionsForCurrentPolicy
  | "TooManyVPCAssociationAuthorizations" ->
      Some TooManyVPCAssociationAuthorizations
  | "TrafficPolicyAlreadyExists" -> Some TrafficPolicyAlreadyExists
  | "TrafficPolicyInUse" -> Some TrafficPolicyInUse
  | "TrafficPolicyInstanceAlreadyExists" ->
      Some TrafficPolicyInstanceAlreadyExists
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "VPCAssociationAuthorizationNotFound" ->
      Some VPCAssociationAuthorizationNotFound
  | "VPCAssociationNotFound" -> Some VPCAssociationNotFound
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None