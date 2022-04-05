type t =
  | AlreadyExistsException 
  | AuthFailure 
  | Blocked 
  | CFNRegistryException 
  | ChangeSetNotFound 
  | ConditionalCheckFailed 
  | CreatedButModifiedException 
  | DryRunOperation 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InsufficientCapabilitiesException 
  | InternalFailure 
  | InvalidAction 
  | InvalidChangeSetStatus 
  | InvalidClientTokenId 
  | InvalidOperationException 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | InvalidStateTransition 
  | LimitExceededException 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | NameAlreadyExistsException 
  | OperationIdAlreadyExistsException 
  | OperationInProgressException 
  | OperationNotFoundException 
  | OptInRequired 
  | PendingVerification 
  | RequestExpired 
  | RequestLimitExceeded 
  | ServiceUnavailable 
  | StackInstanceNotFoundException 
  | StackSetNotEmptyException 
  | StackSetNotFoundException 
  | StaleRequestException 
  | Throttling 
  | TokenAlreadyExistsException 
  | TypeNotFoundException 
  | UnauthorizedOperation 
  | UnknownParameter 
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
  | AlreadyExistsException -> Some 400
  | AuthFailure -> None
  | Blocked -> None
  | CFNRegistryException -> Some 400
  | ChangeSetNotFound -> Some 404
  | ConditionalCheckFailed -> Some 400
  | CreatedButModifiedException -> Some 409
  | DryRunOperation -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InsufficientCapabilitiesException -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidChangeSetStatus -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidOperationException -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidStateTransition -> Some 400
  | LimitExceededException -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NameAlreadyExistsException -> Some 409
  | OperationIdAlreadyExistsException -> Some 409
  | OperationInProgressException -> Some 409
  | OperationNotFoundException -> Some 404
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | StackInstanceNotFoundException -> Some 404
  | StackSetNotEmptyException -> Some 409
  | StackSetNotFoundException -> Some 404
  | StaleRequestException -> Some 409
  | Throttling -> Some 400
  | TokenAlreadyExistsException -> Some 400
  | TypeNotFoundException -> Some 404
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AlreadyExistsException -> "AlreadyExistsException"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | CFNRegistryException -> "CFNRegistryException"
  | ChangeSetNotFound -> "ChangeSetNotFound"
  | ConditionalCheckFailed -> "ConditionalCheckFailed"
  | CreatedButModifiedException -> "CreatedButModifiedException"
  | DryRunOperation -> "DryRunOperation"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InsufficientCapabilitiesException -> "InsufficientCapabilitiesException"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidChangeSetStatus -> "InvalidChangeSetStatus"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidOperationException -> "InvalidOperationException"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidStateTransition -> "InvalidStateTransition"
  | LimitExceededException -> "LimitExceededException"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NameAlreadyExistsException -> "NameAlreadyExistsException"
  | OperationIdAlreadyExistsException -> "OperationIdAlreadyExistsException"
  | OperationInProgressException -> "OperationInProgressException"
  | OperationNotFoundException -> "OperationNotFoundException"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | StackInstanceNotFoundException -> "StackInstanceNotFoundException"
  | StackSetNotEmptyException -> "StackSetNotEmptyException"
  | StackSetNotFoundException -> "StackSetNotFoundException"
  | StaleRequestException -> "StaleRequestException"
  | Throttling -> "Throttling"
  | TokenAlreadyExistsException -> "TokenAlreadyExistsException"
  | TypeNotFoundException -> "TypeNotFoundException"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AlreadyExistsException" -> Some AlreadyExistsException
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CFNRegistryException" -> Some CFNRegistryException
  | "ChangeSetNotFound" -> Some ChangeSetNotFound
  | "ConditionalCheckFailed" -> Some ConditionalCheckFailed
  | "CreatedButModifiedException" -> Some CreatedButModifiedException
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InsufficientCapabilitiesException" ->
      Some InsufficientCapabilitiesException
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidChangeSetStatus" -> Some InvalidChangeSetStatus
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidOperationException" -> Some InvalidOperationException
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidStateTransition" -> Some InvalidStateTransition
  | "LimitExceededException" -> Some LimitExceededException
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NameAlreadyExistsException" -> Some NameAlreadyExistsException
  | "OperationIdAlreadyExistsException" ->
      Some OperationIdAlreadyExistsException
  | "OperationInProgressException" -> Some OperationInProgressException
  | "OperationNotFoundException" -> Some OperationNotFoundException
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "StackInstanceNotFoundException" -> Some StackInstanceNotFoundException
  | "StackSetNotEmptyException" -> Some StackSetNotEmptyException
  | "StackSetNotFoundException" -> Some StackSetNotFoundException
  | "StaleRequestException" -> Some StaleRequestException
  | "Throttling" -> Some Throttling
  | "TokenAlreadyExistsException" -> Some TokenAlreadyExistsException
  | "TypeNotFoundException" -> Some TypeNotFoundException
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None