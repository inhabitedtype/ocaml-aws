type t =
  | AssociatedInstances 
  | AssociationAlreadyExists 
  | AssociationDoesNotExist 
  | AssociationLimitExceeded 
  | AuthFailure 
  | Blocked 
  | DocumentAlreadyExists 
  | DocumentLimitExceeded 
  | DryRunOperation 
  | DuplicateInstanceId 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InternalFailure 
  | InternalServerError 
  | InvalidAction 
  | InvalidClientTokenId 
  | InvalidDocument 
  | InvalidDocumentContent 
  | InvalidInstanceId 
  | InvalidNextToken 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | MalformedQueryString 
  | MaxDocumentSizeExceeded 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | OptInRequired 
  | PendingVerification 
  | RequestExpired 
  | RequestLimitExceeded 
  | ServiceUnavailable 
  | StatusUnchanged 
  | Throttling 
  | TooManyUpdates 
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
  | AssociatedInstances -> Some 400
  | AssociationAlreadyExists -> Some 400
  | AssociationDoesNotExist -> Some 404
  | AssociationLimitExceeded -> Some 400
  | AuthFailure -> None
  | Blocked -> None
  | DocumentAlreadyExists -> Some 400
  | DocumentLimitExceeded -> Some 400
  | DryRunOperation -> None
  | DuplicateInstanceId -> Some 404
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InternalServerError -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidDocument -> Some 404
  | InvalidDocumentContent -> Some 400
  | InvalidInstanceId -> Some 404
  | InvalidNextToken -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | MalformedQueryString -> Some 404
  | MaxDocumentSizeExceeded -> Some 400
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | StatusUnchanged -> Some 400
  | Throttling -> Some 400
  | TooManyUpdates -> Some 429
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AssociatedInstances -> "AssociatedInstances"
  | AssociationAlreadyExists -> "AssociationAlreadyExists"
  | AssociationDoesNotExist -> "AssociationDoesNotExist"
  | AssociationLimitExceeded -> "AssociationLimitExceeded"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | DocumentAlreadyExists -> "DocumentAlreadyExists"
  | DocumentLimitExceeded -> "DocumentLimitExceeded"
  | DryRunOperation -> "DryRunOperation"
  | DuplicateInstanceId -> "DuplicateInstanceId"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InternalServerError -> "InternalServerError"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidDocument -> "InvalidDocument"
  | InvalidDocumentContent -> "InvalidDocumentContent"
  | InvalidInstanceId -> "InvalidInstanceId"
  | InvalidNextToken -> "InvalidNextToken"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | MalformedQueryString -> "MalformedQueryString"
  | MaxDocumentSizeExceeded -> "MaxDocumentSizeExceeded"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | StatusUnchanged -> "StatusUnchanged"
  | Throttling -> "Throttling"
  | TooManyUpdates -> "TooManyUpdates"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AssociatedInstances" -> Some AssociatedInstances
  | "AssociationAlreadyExists" -> Some AssociationAlreadyExists
  | "AssociationDoesNotExist" -> Some AssociationDoesNotExist
  | "AssociationLimitExceeded" -> Some AssociationLimitExceeded
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "DocumentAlreadyExists" -> Some DocumentAlreadyExists
  | "DocumentLimitExceeded" -> Some DocumentLimitExceeded
  | "DryRunOperation" -> Some DryRunOperation
  | "DuplicateInstanceId" -> Some DuplicateInstanceId
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InternalServerError" -> Some InternalServerError
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidDocument" -> Some InvalidDocument
  | "InvalidDocumentContent" -> Some InvalidDocumentContent
  | "InvalidInstanceId" -> Some InvalidInstanceId
  | "InvalidNextToken" -> Some InvalidNextToken
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MaxDocumentSizeExceeded" -> Some MaxDocumentSizeExceeded
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "StatusUnchanged" -> Some StatusUnchanged
  | "Throttling" -> Some Throttling
  | "TooManyUpdates" -> Some TooManyUpdates
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None