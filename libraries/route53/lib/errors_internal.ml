type t =
  | AuthFailure
  | Blocked
  | ConflictingDomainExists
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
  | IdempotentParameterMismatch
  | IncompatibleVersion
  | IncompleteSignature
  | InternalFailure
  | InvalidAction
  | InvalidArgument
  | InvalidChangeBatch
  | InvalidClientTokenId
  | InvalidDomainName
  | InvalidInput
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidVPCId
  | LastVPCAssociation
  | LimitsExceeded
  | MalformedQueryString
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | NoSuchChange
  | NoSuchDelegationSet
  | NoSuchGeoLocation
  | NoSuchHealthCheck
  | NoSuchHostedZone
  | OptInRequired
  | PendingVerification
  | PriorRequestNotComplete
  | PublicZoneVPCAssociation
  | RequestExpired
  | RequestLimitExceeded
  | ServiceUnavailable
  | Throttling
  | ThrottlingException
  | TooManyHealthChecks
  | TooManyHostedZones
  | UnauthorizedOperation
  | UnknownParameter
  | UnsupportedProtocol
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
  | ConflictingDomainExists -> None
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
  | IdempotentParameterMismatch -> None
  | IncompatibleVersion -> Some 400
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidArgument -> None
  | InvalidChangeBatch -> None
  | InvalidClientTokenId -> Some 403
  | InvalidDomainName -> Some 400
  | InvalidInput -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidVPCId -> Some 400
  | LastVPCAssociation -> Some 400
  | LimitsExceeded -> None
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NoSuchChange -> Some 404
  | NoSuchDelegationSet -> None
  | NoSuchGeoLocation -> Some 404
  | NoSuchHealthCheck -> Some 404
  | NoSuchHostedZone -> Some 404
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | PriorRequestNotComplete -> Some 400
  | PublicZoneVPCAssociation -> Some 400
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | ThrottlingException -> Some 400
  | TooManyHealthChecks -> None
  | TooManyHostedZones -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | VPCAssociationNotFound -> Some 404
  | ValidationError -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | ConflictingDomainExists -> "ConflictingDomainExists"
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
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompatibleVersion -> "IncompatibleVersion"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidArgument -> "InvalidArgument"
  | InvalidChangeBatch -> "InvalidChangeBatch"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidDomainName -> "InvalidDomainName"
  | InvalidInput -> "InvalidInput"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidVPCId -> "InvalidVPCId"
  | LastVPCAssociation -> "LastVPCAssociation"
  | LimitsExceeded -> "LimitsExceeded"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NoSuchChange -> "NoSuchChange"
  | NoSuchDelegationSet -> "NoSuchDelegationSet"
  | NoSuchGeoLocation -> "NoSuchGeoLocation"
  | NoSuchHealthCheck -> "NoSuchHealthCheck"
  | NoSuchHostedZone -> "NoSuchHostedZone"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | PriorRequestNotComplete -> "PriorRequestNotComplete"
  | PublicZoneVPCAssociation -> "PublicZoneVPCAssociation"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | ThrottlingException -> "ThrottlingException"
  | TooManyHealthChecks -> "TooManyHealthChecks"
  | TooManyHostedZones -> "TooManyHostedZones"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | VPCAssociationNotFound -> "VPCAssociationNotFound"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "ConflictingDomainExists" -> Some ConflictingDomainExists
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
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompatibleVersion" -> Some IncompatibleVersion
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidArgument" -> Some InvalidArgument
  | "InvalidChangeBatch" -> Some InvalidChangeBatch
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidDomainName" -> Some InvalidDomainName
  | "InvalidInput" -> Some InvalidInput
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidVPCId" -> Some InvalidVPCId
  | "LastVPCAssociation" -> Some LastVPCAssociation
  | "LimitsExceeded" -> Some LimitsExceeded
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NoSuchChange" -> Some NoSuchChange
  | "NoSuchDelegationSet" -> Some NoSuchDelegationSet
  | "NoSuchGeoLocation" -> Some NoSuchGeoLocation
  | "NoSuchHealthCheck" -> Some NoSuchHealthCheck
  | "NoSuchHostedZone" -> Some NoSuchHostedZone
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "PriorRequestNotComplete" -> Some PriorRequestNotComplete
  | "PublicZoneVPCAssociation" -> Some PublicZoneVPCAssociation
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "ThrottlingException" -> Some ThrottlingException
  | "TooManyHealthChecks" -> Some TooManyHealthChecks
  | "TooManyHostedZones" -> Some TooManyHostedZones
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "VPCAssociationNotFound" -> Some VPCAssociationNotFound
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
