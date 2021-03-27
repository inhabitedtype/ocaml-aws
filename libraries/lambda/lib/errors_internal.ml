type t =
  | AuthFailure 
  | Blocked 
  | CodeStorageExceededException 
  | DryRunOperation 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InternalFailure 
  | InvalidAction 
  | InvalidClientTokenId 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidParameterValueException 
  | InvalidQueryParameter 
  | InvalidRequestContentException 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | OptInRequired 
  | PendingVerification 
  | PolicyLengthExceededException 
  | RequestExpired 
  | RequestLimitExceeded 
  | RequestTooLargeException 
  | ResourceConflictException 
  | ResourceNotFoundException 
  | ServiceException 
  | ServiceUnavailable 
  | Throttling 
  | TooManyRequestsException 
  | UnauthorizedOperation 
  | UnknownParameter 
  | UnsupportedMediaTypeException 
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
  | CodeStorageExceededException -> Some 400
  | DryRunOperation -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidParameterValueException -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidRequestContentException -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | PolicyLengthExceededException -> Some 400
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | RequestTooLargeException -> Some 413
  | ResourceConflictException -> Some 409
  | ResourceNotFoundException -> Some 404
  | ServiceException -> Some 500
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | TooManyRequestsException -> Some 429
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedMediaTypeException -> Some 415
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | CodeStorageExceededException -> "CodeStorageExceededException"
  | DryRunOperation -> "DryRunOperation"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidParameterValueException -> "InvalidParameterValueException"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidRequestContentException -> "InvalidRequestContentException"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | PolicyLengthExceededException -> "PolicyLengthExceededException"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | RequestTooLargeException -> "RequestTooLargeException"
  | ResourceConflictException -> "ResourceConflictException"
  | ResourceNotFoundException -> "ResourceNotFoundException"
  | ServiceException -> "ServiceException"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | TooManyRequestsException -> "TooManyRequestsException"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedMediaTypeException -> "UnsupportedMediaTypeException"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CodeStorageExceededException" -> Some CodeStorageExceededException
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidParameterValueException" -> Some InvalidParameterValueException
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidRequestContentException" -> Some InvalidRequestContentException
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "PolicyLengthExceededException" -> Some PolicyLengthExceededException
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "RequestTooLargeException" -> Some RequestTooLargeException
  | "ResourceConflictException" -> Some ResourceConflictException
  | "ResourceNotFoundException" -> Some ResourceNotFoundException
  | "ServiceException" -> Some ServiceException
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "TooManyRequestsException" -> Some TooManyRequestsException
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedMediaTypeException" -> Some UnsupportedMediaTypeException
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None