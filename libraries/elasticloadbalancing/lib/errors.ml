type t =
  | LoadBalancerNotFound
  | AuthFailure
  | Blocked
  | CertificateNotFound
  | DryRunOperation
  | DuplicateLoadBalancerName
  | DuplicateListener
  | DuplicatePolicyName
  | DuplicateTagKeys
  | IdempotentParameterMismatch
  | IncompleteSignature
  | InternalFailure
  | InvalidAction
  | InvalidClientTokenId
  | InvalidConfigurationRequest
  | InvalidInstance
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidScheme
  | InvalidSecurityGroup
  | InvalidSubnet
  | ListenerNotFound
  | LoadBalancerAttributeNotFound
  | MalformedQueryString
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | OptInRequired
  | PendingVerification
  | PolicyNotFound
  | PolicyTypeNotFound
  | RequestExpired
  | RequestLimitExceeded
  | ServiceUnavailable
  | SubnetNotFound
  | Throttling
  | TooManyLoadBalancers
  | TooManyPolicies
  | TooManyTags
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
  | LoadBalancerNotFound  -> Some 400
  | AuthFailure  -> None
  | Blocked  -> None
  | CertificateNotFound  -> Some 400
  | DryRunOperation  -> None
  | DuplicateLoadBalancerName  -> Some 400
  | DuplicateListener  -> Some 400
  | DuplicatePolicyName  -> Some 400
  | DuplicateTagKeys  -> Some 400
  | IdempotentParameterMismatch  -> None
  | IncompleteSignature  -> Some 400
  | InternalFailure  -> Some 500
  | InvalidAction  -> Some 400
  | InvalidClientTokenId  -> Some 403
  | InvalidConfigurationRequest  -> Some 409
  | InvalidInstance  -> Some 400
  | InvalidParameter  -> None
  | InvalidParameterCombination  -> Some 400
  | InvalidParameterValue  -> Some 400
  | InvalidQueryParameter  -> Some 400
  | InvalidScheme  -> Some 400
  | InvalidSecurityGroup  -> Some 400
  | InvalidSubnet  -> Some 400
  | ListenerNotFound  -> Some 400
  | LoadBalancerAttributeNotFound  -> Some 400
  | MalformedQueryString  -> Some 404
  | MissingAction  -> Some 400
  | MissingAuthenticationToken  -> Some 403
  | MissingParameter  -> Some 400
  | OptInRequired  -> Some 403
  | PendingVerification  -> None
  | PolicyNotFound  -> Some 400
  | PolicyTypeNotFound  -> Some 400
  | RequestExpired  -> Some 400
  | RequestLimitExceeded  -> None
  | ServiceUnavailable  -> Some 503
  | SubnetNotFound  -> Some 400
  | Throttling  -> Some 400
  | TooManyLoadBalancers  -> Some 400
  | TooManyPolicies  -> Some 400
  | TooManyTags  -> Some 400
  | UnauthorizedOperation  -> None
  | UnknownParameter  -> None
  | UnsupportedProtocol  -> None
  | ValidationError  -> Some 400
  | Uninhabited  -> None
let to_string e =
  match e with
  | LoadBalancerNotFound  -> "LoadBalancerNotFound"
  | AuthFailure  -> "AuthFailure"
  | Blocked  -> "Blocked"
  | CertificateNotFound  -> "CertificateNotFound"
  | DryRunOperation  -> "DryRunOperation"
  | DuplicateLoadBalancerName  -> "DuplicateLoadBalancerName"
  | DuplicateListener  -> "DuplicateListener"
  | DuplicatePolicyName  -> "DuplicatePolicyName"
  | DuplicateTagKeys  -> "DuplicateTagKeys"
  | IdempotentParameterMismatch  -> "IdempotentParameterMismatch"
  | IncompleteSignature  -> "IncompleteSignature"
  | InternalFailure  -> "InternalFailure"
  | InvalidAction  -> "InvalidAction"
  | InvalidClientTokenId  -> "InvalidClientTokenId"
  | InvalidConfigurationRequest  -> "InvalidConfigurationRequest"
  | InvalidInstance  -> "InvalidInstance"
  | InvalidParameter  -> "InvalidParameter"
  | InvalidParameterCombination  -> "InvalidParameterCombination"
  | InvalidParameterValue  -> "InvalidParameterValue"
  | InvalidQueryParameter  -> "InvalidQueryParameter"
  | InvalidScheme  -> "InvalidScheme"
  | InvalidSecurityGroup  -> "InvalidSecurityGroup"
  | InvalidSubnet  -> "InvalidSubnet"
  | ListenerNotFound  -> "ListenerNotFound"
  | LoadBalancerAttributeNotFound  -> "LoadBalancerAttributeNotFound"
  | MalformedQueryString  -> "MalformedQueryString"
  | MissingAction  -> "MissingAction"
  | MissingAuthenticationToken  -> "MissingAuthenticationToken"
  | MissingParameter  -> "MissingParameter"
  | OptInRequired  -> "OptInRequired"
  | PendingVerification  -> "PendingVerification"
  | PolicyNotFound  -> "PolicyNotFound"
  | PolicyTypeNotFound  -> "PolicyTypeNotFound"
  | RequestExpired  -> "RequestExpired"
  | RequestLimitExceeded  -> "RequestLimitExceeded"
  | ServiceUnavailable  -> "ServiceUnavailable"
  | SubnetNotFound  -> "SubnetNotFound"
  | Throttling  -> "Throttling"
  | TooManyLoadBalancers  -> "TooManyLoadBalancers"
  | TooManyPolicies  -> "TooManyPolicies"
  | TooManyTags  -> "TooManyTags"
  | UnauthorizedOperation  -> "UnauthorizedOperation"
  | UnknownParameter  -> "UnknownParameter"
  | UnsupportedProtocol  -> "UnsupportedProtocol"
  | ValidationError  -> "ValidationError"
  | Uninhabited  -> "Uninhabited"
let of_string e =
  match e with
  | "LoadBalancerNotFound" -> Some LoadBalancerNotFound
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "CertificateNotFound" -> Some CertificateNotFound
  | "DryRunOperation" -> Some DryRunOperation
  | "DuplicateLoadBalancerName" -> Some DuplicateLoadBalancerName
  | "DuplicateListener" -> Some DuplicateListener
  | "DuplicatePolicyName" -> Some DuplicatePolicyName
  | "DuplicateTagKeys" -> Some DuplicateTagKeys
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidConfigurationRequest" -> Some InvalidConfigurationRequest
  | "InvalidInstance" -> Some InvalidInstance
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidScheme" -> Some InvalidScheme
  | "InvalidSecurityGroup" -> Some InvalidSecurityGroup
  | "InvalidSubnet" -> Some InvalidSubnet
  | "ListenerNotFound" -> Some ListenerNotFound
  | "LoadBalancerAttributeNotFound" -> Some LoadBalancerAttributeNotFound
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "PolicyNotFound" -> Some PolicyNotFound
  | "PolicyTypeNotFound" -> Some PolicyTypeNotFound
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "SubnetNotFound" -> Some SubnetNotFound
  | "Throttling" -> Some Throttling
  | "TooManyLoadBalancers" -> Some TooManyLoadBalancers
  | "TooManyPolicies" -> Some TooManyPolicies
  | "TooManyTags" -> Some TooManyTags
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None