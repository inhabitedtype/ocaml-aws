type t =
  | AuthFailure
  | Blocked
  | DryRunOperation
  | ExpiredTokenException
  | ExpiredTradeInTokenException
  | IDPCommunicationError
  | IDPRejectedClaim
  | IdempotentParameterMismatch
  | IncompleteSignature
  | InternalFailure
  | InvalidAction
  | InvalidAuthorizationMessageException
  | InvalidClientTokenId
  | InvalidIdentityToken
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | JWTPayloadSizeExceededException
  | MalformedPolicyDocument
  | MalformedQueryString
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | OptInRequired
  | OutboundWebIdentityFederationDisabledException
  | PackedPolicyTooLarge
  | PendingVerification
  | RegionDisabledException
  | RequestExpired
  | RequestLimitExceeded
  | ServiceUnavailable
  | SessionDurationEscalationException
  | Throttling
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
  | DryRunOperation -> None
  | ExpiredTokenException -> Some 400
  | ExpiredTradeInTokenException -> Some 400
  | IDPCommunicationError -> Some 400
  | IDPRejectedClaim -> Some 403
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidAuthorizationMessageException -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidIdentityToken -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | JWTPayloadSizeExceededException -> Some 400
  | MalformedPolicyDocument -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OptInRequired -> Some 403
  | OutboundWebIdentityFederationDisabledException -> Some 403
  | PackedPolicyTooLarge -> Some 400
  | PendingVerification -> None
  | RegionDisabledException -> Some 403
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | SessionDurationEscalationException -> Some 403
  | Throttling -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | DryRunOperation -> "DryRunOperation"
  | ExpiredTokenException -> "ExpiredTokenException"
  | ExpiredTradeInTokenException -> "ExpiredTradeInTokenException"
  | IDPCommunicationError -> "IDPCommunicationError"
  | IDPRejectedClaim -> "IDPRejectedClaim"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidAuthorizationMessageException -> "InvalidAuthorizationMessageException"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidIdentityToken -> "InvalidIdentityToken"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | JWTPayloadSizeExceededException -> "JWTPayloadSizeExceededException"
  | MalformedPolicyDocument -> "MalformedPolicyDocument"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OptInRequired -> "OptInRequired"
  | OutboundWebIdentityFederationDisabledException ->
      "OutboundWebIdentityFederationDisabledException"
  | PackedPolicyTooLarge -> "PackedPolicyTooLarge"
  | PendingVerification -> "PendingVerification"
  | RegionDisabledException -> "RegionDisabledException"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | SessionDurationEscalationException -> "SessionDurationEscalationException"
  | Throttling -> "Throttling"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "DryRunOperation" -> Some DryRunOperation
  | "ExpiredTokenException" -> Some ExpiredTokenException
  | "ExpiredTradeInTokenException" -> Some ExpiredTradeInTokenException
  | "IDPCommunicationError" -> Some IDPCommunicationError
  | "IDPRejectedClaim" -> Some IDPRejectedClaim
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidAuthorizationMessageException" -> Some InvalidAuthorizationMessageException
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidIdentityToken" -> Some InvalidIdentityToken
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "JWTPayloadSizeExceededException" -> Some JWTPayloadSizeExceededException
  | "MalformedPolicyDocument" -> Some MalformedPolicyDocument
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "OutboundWebIdentityFederationDisabledException" ->
      Some OutboundWebIdentityFederationDisabledException
  | "PackedPolicyTooLarge" -> Some PackedPolicyTooLarge
  | "PendingVerification" -> Some PendingVerification
  | "RegionDisabledException" -> Some RegionDisabledException
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "SessionDurationEscalationException" -> Some SessionDurationEscalationException
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
