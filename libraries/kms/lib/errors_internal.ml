type t =
  | AlreadyExistsException
  | AuthFailure
  | Blocked
  | CloudHsmClusterInUseException
  | CloudHsmClusterInvalidConfigurationException
  | CloudHsmClusterNotActiveException
  | CloudHsmClusterNotFoundException
  | CloudHsmClusterNotRelatedException
  | CustomKeyStoreHasCMKsException
  | CustomKeyStoreInvalidStateException
  | CustomKeyStoreNameInUseException
  | CustomKeyStoreNotFoundException
  | DependencyTimeoutException
  | DisabledException
  | DryRunOperation
  | ExpiredImportTokenException
  | IdempotentParameterMismatch
  | IncompleteSignature
  | IncorrectKeyException
  | IncorrectKeyMaterialException
  | IncorrectTrustAnchorException
  | InternalFailure
  | InvalidAction
  | InvalidAliasNameException
  | InvalidArnException
  | InvalidCiphertextException
  | InvalidClientTokenId
  | InvalidGrantIdException
  | InvalidGrantTokenException
  | InvalidImportTokenException
  | InvalidKeyUsageException
  | InvalidMarkerException
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | KMSInternalException
  | KMSInvalidSignatureException
  | KMSInvalidStateException
  | KeyUnavailableException
  | LimitExceededException
  | MalformedPolicyDocumentException
  | MalformedQueryString
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | NotFoundException
  | OptInRequired
  | PendingVerification
  | RequestExpired
  | RequestLimitExceeded
  | ServiceUnavailable
  | TagException
  | Throttling
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
  | AlreadyExistsException -> None
  | AuthFailure -> None
  | Blocked -> None
  | CloudHsmClusterInUseException -> None
  | CloudHsmClusterInvalidConfigurationException -> None
  | CloudHsmClusterNotActiveException -> None
  | CloudHsmClusterNotFoundException -> None
  | CloudHsmClusterNotRelatedException -> None
  | CustomKeyStoreHasCMKsException -> None
  | CustomKeyStoreInvalidStateException -> None
  | CustomKeyStoreNameInUseException -> None
  | CustomKeyStoreNotFoundException -> None
  | DependencyTimeoutException -> None
  | DisabledException -> None
  | DryRunOperation -> None
  | ExpiredImportTokenException -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | IncorrectKeyException -> None
  | IncorrectKeyMaterialException -> None
  | IncorrectTrustAnchorException -> None
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidAliasNameException -> None
  | InvalidArnException -> None
  | InvalidCiphertextException -> None
  | InvalidClientTokenId -> Some 403
  | InvalidGrantIdException -> None
  | InvalidGrantTokenException -> None
  | InvalidImportTokenException -> None
  | InvalidKeyUsageException -> None
  | InvalidMarkerException -> None
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | KMSInternalException -> None
  | KMSInvalidSignatureException -> None
  | KMSInvalidStateException -> None
  | KeyUnavailableException -> None
  | LimitExceededException -> None
  | MalformedPolicyDocumentException -> None
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NotFoundException -> None
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | TagException -> None
  | Throttling -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedOperationException -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AlreadyExistsException -> "AlreadyExistsException"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | CloudHsmClusterInUseException -> "CloudHsmClusterInUseException"
  | CloudHsmClusterInvalidConfigurationException ->
      "CloudHsmClusterInvalidConfigurationException"
  | CloudHsmClusterNotActiveException -> "CloudHsmClusterNotActiveException"
  | CloudHsmClusterNotFoundException -> "CloudHsmClusterNotFoundException"
  | CloudHsmClusterNotRelatedException -> "CloudHsmClusterNotRelatedException"
  | CustomKeyStoreHasCMKsException -> "CustomKeyStoreHasCMKsException"
  | CustomKeyStoreInvalidStateException -> "CustomKeyStoreInvalidStateException"
  | CustomKeyStoreNameInUseException -> "CustomKeyStoreNameInUseException"
  | CustomKeyStoreNotFoundException -> "CustomKeyStoreNotFoundException"
  | DependencyTimeoutException -> "DependencyTimeoutException"
  | DisabledException -> "DisabledException"
  | DryRunOperation -> "DryRunOperation"
  | ExpiredImportTokenException -> "ExpiredImportTokenException"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | IncorrectKeyException -> "IncorrectKeyException"
  | IncorrectKeyMaterialException -> "IncorrectKeyMaterialException"
  | IncorrectTrustAnchorException -> "IncorrectTrustAnchorException"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidAliasNameException -> "InvalidAliasNameException"
  | InvalidArnException -> "InvalidArnException"
  | InvalidCiphertextException -> "InvalidCiphertextException"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidGrantIdException -> "InvalidGrantIdException"
  | InvalidGrantTokenException -> "InvalidGrantTokenException"
  | InvalidImportTokenException -> "InvalidImportTokenException"
  | InvalidKeyUsageException -> "InvalidKeyUsageException"
  | InvalidMarkerException -> "InvalidMarkerException"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | KMSInternalException -> "KMSInternalException"
  | KMSInvalidSignatureException -> "KMSInvalidSignatureException"
  | KMSInvalidStateException -> "KMSInvalidStateException"
  | KeyUnavailableException -> "KeyUnavailableException"
  | LimitExceededException -> "LimitExceededException"
  | MalformedPolicyDocumentException -> "MalformedPolicyDocumentException"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NotFoundException -> "NotFoundException"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | TagException -> "TagException"
  | Throttling -> "Throttling"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedOperationException -> "UnsupportedOperationException"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AlreadyExistsException" -> Some AlreadyExistsException
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CloudHsmClusterInUseException" -> Some CloudHsmClusterInUseException
  | "CloudHsmClusterInvalidConfigurationException" ->
      Some CloudHsmClusterInvalidConfigurationException
  | "CloudHsmClusterNotActiveException" -> Some CloudHsmClusterNotActiveException
  | "CloudHsmClusterNotFoundException" -> Some CloudHsmClusterNotFoundException
  | "CloudHsmClusterNotRelatedException" -> Some CloudHsmClusterNotRelatedException
  | "CustomKeyStoreHasCMKsException" -> Some CustomKeyStoreHasCMKsException
  | "CustomKeyStoreInvalidStateException" -> Some CustomKeyStoreInvalidStateException
  | "CustomKeyStoreNameInUseException" -> Some CustomKeyStoreNameInUseException
  | "CustomKeyStoreNotFoundException" -> Some CustomKeyStoreNotFoundException
  | "DependencyTimeoutException" -> Some DependencyTimeoutException
  | "DisabledException" -> Some DisabledException
  | "DryRunOperation" -> Some DryRunOperation
  | "ExpiredImportTokenException" -> Some ExpiredImportTokenException
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "IncorrectKeyException" -> Some IncorrectKeyException
  | "IncorrectKeyMaterialException" -> Some IncorrectKeyMaterialException
  | "IncorrectTrustAnchorException" -> Some IncorrectTrustAnchorException
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidAliasNameException" -> Some InvalidAliasNameException
  | "InvalidArnException" -> Some InvalidArnException
  | "InvalidCiphertextException" -> Some InvalidCiphertextException
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidGrantIdException" -> Some InvalidGrantIdException
  | "InvalidGrantTokenException" -> Some InvalidGrantTokenException
  | "InvalidImportTokenException" -> Some InvalidImportTokenException
  | "InvalidKeyUsageException" -> Some InvalidKeyUsageException
  | "InvalidMarkerException" -> Some InvalidMarkerException
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "KMSInternalException" -> Some KMSInternalException
  | "KMSInvalidSignatureException" -> Some KMSInvalidSignatureException
  | "KMSInvalidStateException" -> Some KMSInvalidStateException
  | "KeyUnavailableException" -> Some KeyUnavailableException
  | "LimitExceededException" -> Some LimitExceededException
  | "MalformedPolicyDocumentException" -> Some MalformedPolicyDocumentException
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NotFoundException" -> Some NotFoundException
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "TagException" -> Some TagException
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedOperationException" -> Some UnsupportedOperationException
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
