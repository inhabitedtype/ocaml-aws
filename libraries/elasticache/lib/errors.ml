type t =
  | AuthFailure
  | AuthorizationAlreadyExists
  | AuthorizationNotFound
  | Blocked
  | CacheClusterAlreadyExists
  | CacheClusterNotFound
  | CacheParameterGroupAlreadyExists
  | CacheParameterGroupNotFound
  | CacheParameterGroupQuotaExceeded
  | CacheSecurityGroupAlreadyExists
  | CacheSecurityGroupNotFound
  | QuotaExceeded_CacheSecurityGroup
  | CacheSubnetGroupAlreadyExists
  | CacheSubnetGroupInUse
  | CacheSubnetGroupNotFoundFault
  | CacheSubnetGroupQuotaExceeded
  | CacheSubnetQuotaExceededFault
  | ClusterQuotaForCustomerExceeded
  | DryRunOperation
  | IdempotentParameterMismatch
  | IncompleteSignature
  | InsufficientCacheClusterCapacity
  | InternalFailure
  | InvalidARN
  | InvalidAction
  | InvalidCacheClusterState
  | InvalidCacheParameterGroupState
  | InvalidCacheSecurityGroupState
  | InvalidClientTokenId
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidReplicationGroupState
  | InvalidSnapshotState
  | InvalidSubnet
  | InvalidVPCNetworkStateFault
  | MalformedQueryString
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | NodeQuotaForClusterExceeded
  | NodeQuotaForCustomerExceeded
  | OptInRequired
  | PendingVerification
  | ReplicationGroupAlreadyExists
  | ReplicationGroupNotFoundFault
  | RequestExpired
  | RequestLimitExceeded
  | ReservedCacheNodeAlreadyExists
  | ReservedCacheNodeNotFound
  | ReservedCacheNodeQuotaExceeded
  | ReservedCacheNodesOfferingNotFound
  | ServiceUnavailable
  | SnapshotAlreadyExistsFault
  | SnapshotFeatureNotSupportedFault
  | SnapshotNotFoundFault
  | SnapshotQuotaExceededFault
  | SubnetInUse
  | TagNotFound
  | TagQuotaPerResourceExceeded
  | Throttling
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
  | AuthFailure  -> None
  | AuthorizationAlreadyExists  -> Some 400
  | AuthorizationNotFound  -> Some 404
  | Blocked  -> None
  | CacheClusterAlreadyExists  -> Some 400
  | CacheClusterNotFound  -> Some 404
  | CacheParameterGroupAlreadyExists  -> Some 400
  | CacheParameterGroupNotFound  -> Some 404
  | CacheParameterGroupQuotaExceeded  -> Some 400
  | CacheSecurityGroupAlreadyExists  -> Some 400
  | CacheSecurityGroupNotFound  -> Some 404
  | QuotaExceeded_CacheSecurityGroup  -> Some 400
  | CacheSubnetGroupAlreadyExists  -> Some 400
  | CacheSubnetGroupInUse  -> Some 400
  | CacheSubnetGroupNotFoundFault  -> Some 400
  | CacheSubnetGroupQuotaExceeded  -> Some 400
  | CacheSubnetQuotaExceededFault  -> Some 400
  | ClusterQuotaForCustomerExceeded  -> Some 400
  | DryRunOperation  -> None
  | IdempotentParameterMismatch  -> None
  | IncompleteSignature  -> Some 400
  | InsufficientCacheClusterCapacity  -> Some 400
  | InternalFailure  -> Some 500
  | InvalidARN  -> Some 400
  | InvalidAction  -> Some 400
  | InvalidCacheClusterState  -> Some 400
  | InvalidCacheParameterGroupState  -> Some 400
  | InvalidCacheSecurityGroupState  -> Some 400
  | InvalidClientTokenId  -> Some 403
  | InvalidParameter  -> None
  | InvalidParameterCombination  -> Some 400
  | InvalidParameterCombination  -> Some 400
  | InvalidParameterValue  -> Some 400
  | InvalidParameterValue  -> Some 400
  | InvalidQueryParameter  -> Some 400
  | InvalidReplicationGroupState  -> Some 400
  | InvalidSnapshotState  -> Some 400
  | InvalidSubnet  -> Some 400
  | InvalidVPCNetworkStateFault  -> Some 400
  | MalformedQueryString  -> Some 404
  | MissingAction  -> Some 400
  | MissingAuthenticationToken  -> Some 403
  | MissingParameter  -> Some 400
  | NodeQuotaForClusterExceeded  -> Some 400
  | NodeQuotaForCustomerExceeded  -> Some 400
  | OptInRequired  -> Some 403
  | PendingVerification  -> None
  | ReplicationGroupAlreadyExists  -> Some 400
  | ReplicationGroupNotFoundFault  -> Some 404
  | RequestExpired  -> Some 400
  | RequestLimitExceeded  -> None
  | ReservedCacheNodeAlreadyExists  -> Some 404
  | ReservedCacheNodeNotFound  -> Some 404
  | ReservedCacheNodeQuotaExceeded  -> Some 400
  | ReservedCacheNodesOfferingNotFound  -> Some 404
  | ServiceUnavailable  -> Some 503
  | SnapshotAlreadyExistsFault  -> Some 400
  | SnapshotFeatureNotSupportedFault  -> Some 400
  | SnapshotNotFoundFault  -> Some 404
  | SnapshotQuotaExceededFault  -> Some 400
  | SubnetInUse  -> Some 400
  | TagNotFound  -> Some 404
  | TagQuotaPerResourceExceeded  -> Some 400
  | Throttling  -> Some 400
  | UnauthorizedOperation  -> None
  | UnknownParameter  -> None
  | UnsupportedProtocol  -> None
  | ValidationError  -> Some 400
  | Uninhabited  -> None
let to_string e =
  match e with
  | AuthFailure  -> "AuthFailure"
  | AuthorizationAlreadyExists  -> "AuthorizationAlreadyExists"
  | AuthorizationNotFound  -> "AuthorizationNotFound"
  | Blocked  -> "Blocked"
  | CacheClusterAlreadyExists  -> "CacheClusterAlreadyExists"
  | CacheClusterNotFound  -> "CacheClusterNotFound"
  | CacheParameterGroupAlreadyExists  -> "CacheParameterGroupAlreadyExists"
  | CacheParameterGroupNotFound  -> "CacheParameterGroupNotFound"
  | CacheParameterGroupQuotaExceeded  -> "CacheParameterGroupQuotaExceeded"
  | CacheSecurityGroupAlreadyExists  -> "CacheSecurityGroupAlreadyExists"
  | CacheSecurityGroupNotFound  -> "CacheSecurityGroupNotFound"
  | QuotaExceeded_CacheSecurityGroup  -> "QuotaExceeded.CacheSecurityGroup"
  | CacheSubnetGroupAlreadyExists  -> "CacheSubnetGroupAlreadyExists"
  | CacheSubnetGroupInUse  -> "CacheSubnetGroupInUse"
  | CacheSubnetGroupNotFoundFault  -> "CacheSubnetGroupNotFoundFault"
  | CacheSubnetGroupQuotaExceeded  -> "CacheSubnetGroupQuotaExceeded"
  | CacheSubnetQuotaExceededFault  -> "CacheSubnetQuotaExceededFault"
  | ClusterQuotaForCustomerExceeded  -> "ClusterQuotaForCustomerExceeded"
  | DryRunOperation  -> "DryRunOperation"
  | IdempotentParameterMismatch  -> "IdempotentParameterMismatch"
  | IncompleteSignature  -> "IncompleteSignature"
  | InsufficientCacheClusterCapacity  -> "InsufficientCacheClusterCapacity"
  | InternalFailure  -> "InternalFailure"
  | InvalidARN  -> "InvalidARN"
  | InvalidAction  -> "InvalidAction"
  | InvalidCacheClusterState  -> "InvalidCacheClusterState"
  | InvalidCacheParameterGroupState  -> "InvalidCacheParameterGroupState"
  | InvalidCacheSecurityGroupState  -> "InvalidCacheSecurityGroupState"
  | InvalidClientTokenId  -> "InvalidClientTokenId"
  | InvalidParameter  -> "InvalidParameter"
  | InvalidParameterCombination  -> "InvalidParameterCombination"
  | InvalidParameterCombination  -> "InvalidParameterCombination"
  | InvalidParameterValue  -> "InvalidParameterValue"
  | InvalidParameterValue  -> "InvalidParameterValue"
  | InvalidQueryParameter  -> "InvalidQueryParameter"
  | InvalidReplicationGroupState  -> "InvalidReplicationGroupState"
  | InvalidSnapshotState  -> "InvalidSnapshotState"
  | InvalidSubnet  -> "InvalidSubnet"
  | InvalidVPCNetworkStateFault  -> "InvalidVPCNetworkStateFault"
  | MalformedQueryString  -> "MalformedQueryString"
  | MissingAction  -> "MissingAction"
  | MissingAuthenticationToken  -> "MissingAuthenticationToken"
  | MissingParameter  -> "MissingParameter"
  | NodeQuotaForClusterExceeded  -> "NodeQuotaForClusterExceeded"
  | NodeQuotaForCustomerExceeded  -> "NodeQuotaForCustomerExceeded"
  | OptInRequired  -> "OptInRequired"
  | PendingVerification  -> "PendingVerification"
  | ReplicationGroupAlreadyExists  -> "ReplicationGroupAlreadyExists"
  | ReplicationGroupNotFoundFault  -> "ReplicationGroupNotFoundFault"
  | RequestExpired  -> "RequestExpired"
  | RequestLimitExceeded  -> "RequestLimitExceeded"
  | ReservedCacheNodeAlreadyExists  -> "ReservedCacheNodeAlreadyExists"
  | ReservedCacheNodeNotFound  -> "ReservedCacheNodeNotFound"
  | ReservedCacheNodeQuotaExceeded  -> "ReservedCacheNodeQuotaExceeded"
  | ReservedCacheNodesOfferingNotFound  ->
      "ReservedCacheNodesOfferingNotFound"
  | ServiceUnavailable  -> "ServiceUnavailable"
  | SnapshotAlreadyExistsFault  -> "SnapshotAlreadyExistsFault"
  | SnapshotFeatureNotSupportedFault  -> "SnapshotFeatureNotSupportedFault"
  | SnapshotNotFoundFault  -> "SnapshotNotFoundFault"
  | SnapshotQuotaExceededFault  -> "SnapshotQuotaExceededFault"
  | SubnetInUse  -> "SubnetInUse"
  | TagNotFound  -> "TagNotFound"
  | TagQuotaPerResourceExceeded  -> "TagQuotaPerResourceExceeded"
  | Throttling  -> "Throttling"
  | UnauthorizedOperation  -> "UnauthorizedOperation"
  | UnknownParameter  -> "UnknownParameter"
  | UnsupportedProtocol  -> "UnsupportedProtocol"
  | ValidationError  -> "ValidationError"
  | Uninhabited  -> "Uninhabited"
let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "AuthorizationAlreadyExists" -> Some AuthorizationAlreadyExists
  | "AuthorizationNotFound" -> Some AuthorizationNotFound
  | "Blocked" -> Some Blocked
  | "CacheClusterAlreadyExists" -> Some CacheClusterAlreadyExists
  | "CacheClusterNotFound" -> Some CacheClusterNotFound
  | "CacheParameterGroupAlreadyExists" ->
      Some CacheParameterGroupAlreadyExists
  | "CacheParameterGroupNotFound" -> Some CacheParameterGroupNotFound
  | "CacheParameterGroupQuotaExceeded" ->
      Some CacheParameterGroupQuotaExceeded
  | "CacheSecurityGroupAlreadyExists" -> Some CacheSecurityGroupAlreadyExists
  | "CacheSecurityGroupNotFound" -> Some CacheSecurityGroupNotFound
  | "QuotaExceeded.CacheSecurityGroup" ->
      Some QuotaExceeded_CacheSecurityGroup
  | "CacheSubnetGroupAlreadyExists" -> Some CacheSubnetGroupAlreadyExists
  | "CacheSubnetGroupInUse" -> Some CacheSubnetGroupInUse
  | "CacheSubnetGroupNotFoundFault" -> Some CacheSubnetGroupNotFoundFault
  | "CacheSubnetGroupQuotaExceeded" -> Some CacheSubnetGroupQuotaExceeded
  | "CacheSubnetQuotaExceededFault" -> Some CacheSubnetQuotaExceededFault
  | "ClusterQuotaForCustomerExceeded" -> Some ClusterQuotaForCustomerExceeded
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InsufficientCacheClusterCapacity" ->
      Some InsufficientCacheClusterCapacity
  | "InternalFailure" -> Some InternalFailure
  | "InvalidARN" -> Some InvalidARN
  | "InvalidAction" -> Some InvalidAction
  | "InvalidCacheClusterState" -> Some InvalidCacheClusterState
  | "InvalidCacheParameterGroupState" -> Some InvalidCacheParameterGroupState
  | "InvalidCacheSecurityGroupState" -> Some InvalidCacheSecurityGroupState
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidReplicationGroupState" -> Some InvalidReplicationGroupState
  | "InvalidSnapshotState" -> Some InvalidSnapshotState
  | "InvalidSubnet" -> Some InvalidSubnet
  | "InvalidVPCNetworkStateFault" -> Some InvalidVPCNetworkStateFault
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NodeQuotaForClusterExceeded" -> Some NodeQuotaForClusterExceeded
  | "NodeQuotaForCustomerExceeded" -> Some NodeQuotaForCustomerExceeded
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "ReplicationGroupAlreadyExists" -> Some ReplicationGroupAlreadyExists
  | "ReplicationGroupNotFoundFault" -> Some ReplicationGroupNotFoundFault
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ReservedCacheNodeAlreadyExists" -> Some ReservedCacheNodeAlreadyExists
  | "ReservedCacheNodeNotFound" -> Some ReservedCacheNodeNotFound
  | "ReservedCacheNodeQuotaExceeded" -> Some ReservedCacheNodeQuotaExceeded
  | "ReservedCacheNodesOfferingNotFound" ->
      Some ReservedCacheNodesOfferingNotFound
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "SnapshotAlreadyExistsFault" -> Some SnapshotAlreadyExistsFault
  | "SnapshotFeatureNotSupportedFault" ->
      Some SnapshotFeatureNotSupportedFault
  | "SnapshotNotFoundFault" -> Some SnapshotNotFoundFault
  | "SnapshotQuotaExceededFault" -> Some SnapshotQuotaExceededFault
  | "SubnetInUse" -> Some SubnetInUse
  | "TagNotFound" -> Some TagNotFound
  | "TagQuotaPerResourceExceeded" -> Some TagQuotaPerResourceExceeded
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None