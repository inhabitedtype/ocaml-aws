type t =
  | AccessDeniedException 
  | AuthFailure 
  | Blocked 
  | DryRunOperation 
  | IdempotentParameterMismatch 
  | IdempotentParameterMismatchException 
  | ImageTooLargeException 
  | IncompleteSignature 
  | InternalFailure 
  | InternalServerError 
  | InvalidAction 
  | InvalidClientTokenId 
  | InvalidImageFormatException 
  | InvalidPaginationTokenException 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterException 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | InvalidS3ObjectException 
  | LimitExceededException 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | OptInRequired 
  | PendingVerification 
  | ProvisionedThroughputExceededException 
  | RequestExpired 
  | RequestLimitExceeded 
  | ResourceAlreadyExistsException 
  | ResourceInUseException 
  | ResourceNotFoundException 
  | ServiceUnavailable 
  | Throttling 
  | ThrottlingException 
  | UnauthorizedOperation 
  | UnknownParameter 
  | UnsupportedProtocol 
  | ValidationError 
  | VideoTooLargeException 
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
  | AccessDeniedException -> None
  | AuthFailure -> None
  | Blocked -> None
  | DryRunOperation -> None
  | IdempotentParameterMismatch -> None
  | IdempotentParameterMismatchException -> None
  | ImageTooLargeException -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InternalServerError -> None
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidImageFormatException -> None
  | InvalidPaginationTokenException -> None
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterException -> None
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidS3ObjectException -> None
  | LimitExceededException -> None
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | ProvisionedThroughputExceededException -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ResourceAlreadyExistsException -> None
  | ResourceInUseException -> None
  | ResourceNotFoundException -> None
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | ThrottlingException -> None
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | VideoTooLargeException -> None
  | Uninhabited -> None
let to_string e =
  match e with
  | AccessDeniedException -> "AccessDeniedException"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | DryRunOperation -> "DryRunOperation"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IdempotentParameterMismatchException ->
      "IdempotentParameterMismatchException"
  | ImageTooLargeException -> "ImageTooLargeException"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InternalServerError -> "InternalServerError"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidImageFormatException -> "InvalidImageFormatException"
  | InvalidPaginationTokenException -> "InvalidPaginationTokenException"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterException -> "InvalidParameterException"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidS3ObjectException -> "InvalidS3ObjectException"
  | LimitExceededException -> "LimitExceededException"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | ProvisionedThroughputExceededException ->
      "ProvisionedThroughputExceededException"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ResourceAlreadyExistsException -> "ResourceAlreadyExistsException"
  | ResourceInUseException -> "ResourceInUseException"
  | ResourceNotFoundException -> "ResourceNotFoundException"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | ThrottlingException -> "ThrottlingException"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | VideoTooLargeException -> "VideoTooLargeException"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AccessDeniedException" -> Some AccessDeniedException
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IdempotentParameterMismatchException" ->
      Some IdempotentParameterMismatchException
  | "ImageTooLargeException" -> Some ImageTooLargeException
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InternalServerError" -> Some InternalServerError
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidImageFormatException" -> Some InvalidImageFormatException
  | "InvalidPaginationTokenException" -> Some InvalidPaginationTokenException
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterException" -> Some InvalidParameterException
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidS3ObjectException" -> Some InvalidS3ObjectException
  | "LimitExceededException" -> Some LimitExceededException
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "ProvisionedThroughputExceededException" ->
      Some ProvisionedThroughputExceededException
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ResourceAlreadyExistsException" -> Some ResourceAlreadyExistsException
  | "ResourceInUseException" -> Some ResourceInUseException
  | "ResourceNotFoundException" -> Some ResourceNotFoundException
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "ThrottlingException" -> Some ThrottlingException
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "VideoTooLargeException" -> Some VideoTooLargeException
  | "Uninhabited" -> Some Uninhabited
  | _ -> None