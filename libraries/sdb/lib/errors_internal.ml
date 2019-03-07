type t =
  | AttributeDoesNotExist 
  | AuthFailure 
  | Blocked 
  | DryRunOperation 
  | DuplicateItemName 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InternalFailure 
  | InvalidAction 
  | InvalidClientTokenId 
  | InvalidNextToken 
  | InvalidNumberPredicates 
  | InvalidNumberValueTests 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryExpression 
  | InvalidQueryParameter 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | NoSuchDomain 
  | NumberDomainAttributesExceeded 
  | NumberDomainBytesExceeded 
  | NumberDomainsExceeded 
  | NumberItemAttributesExceeded 
  | NumberSubmittedAttributesExceeded 
  | NumberSubmittedItemsExceeded 
  | OptInRequired 
  | PendingVerification 
  | RequestExpired 
  | RequestLimitExceeded 
  | RequestTimeout 
  | ServiceUnavailable 
  | Throttling 
  | TooManyRequestedAttributes 
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
  | AttributeDoesNotExist -> Some 404
  | AuthFailure -> None
  | Blocked -> None
  | DryRunOperation -> None
  | DuplicateItemName -> Some 400
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidNextToken -> Some 400
  | InvalidNumberPredicates -> Some 400
  | InvalidNumberValueTests -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryExpression -> Some 400
  | InvalidQueryParameter -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NoSuchDomain -> Some 400
  | NumberDomainAttributesExceeded -> Some 409
  | NumberDomainBytesExceeded -> Some 409
  | NumberDomainsExceeded -> Some 409
  | NumberItemAttributesExceeded -> Some 409
  | NumberSubmittedAttributesExceeded -> Some 409
  | NumberSubmittedItemsExceeded -> Some 409
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | RequestTimeout -> Some 408
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | TooManyRequestedAttributes -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AttributeDoesNotExist -> "AttributeDoesNotExist"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | DryRunOperation -> "DryRunOperation"
  | DuplicateItemName -> "DuplicateItemName"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidNextToken -> "InvalidNextToken"
  | InvalidNumberPredicates -> "InvalidNumberPredicates"
  | InvalidNumberValueTests -> "InvalidNumberValueTests"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryExpression -> "InvalidQueryExpression"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NoSuchDomain -> "NoSuchDomain"
  | NumberDomainAttributesExceeded -> "NumberDomainAttributesExceeded"
  | NumberDomainBytesExceeded -> "NumberDomainBytesExceeded"
  | NumberDomainsExceeded -> "NumberDomainsExceeded"
  | NumberItemAttributesExceeded -> "NumberItemAttributesExceeded"
  | NumberSubmittedAttributesExceeded -> "NumberSubmittedAttributesExceeded"
  | NumberSubmittedItemsExceeded -> "NumberSubmittedItemsExceeded"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | RequestTimeout -> "RequestTimeout"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | TooManyRequestedAttributes -> "TooManyRequestedAttributes"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AttributeDoesNotExist" -> Some AttributeDoesNotExist
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "DryRunOperation" -> Some DryRunOperation
  | "DuplicateItemName" -> Some DuplicateItemName
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidNextToken" -> Some InvalidNextToken
  | "InvalidNumberPredicates" -> Some InvalidNumberPredicates
  | "InvalidNumberValueTests" -> Some InvalidNumberValueTests
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryExpression" -> Some InvalidQueryExpression
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NoSuchDomain" -> Some NoSuchDomain
  | "NumberDomainAttributesExceeded" -> Some NumberDomainAttributesExceeded
  | "NumberDomainBytesExceeded" -> Some NumberDomainBytesExceeded
  | "NumberDomainsExceeded" -> Some NumberDomainsExceeded
  | "NumberItemAttributesExceeded" -> Some NumberItemAttributesExceeded
  | "NumberSubmittedAttributesExceeded" ->
      Some NumberSubmittedAttributesExceeded
  | "NumberSubmittedItemsExceeded" -> Some NumberSubmittedItemsExceeded
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "RequestTimeout" -> Some RequestTimeout
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "TooManyRequestedAttributes" -> Some TooManyRequestedAttributes
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None