type t =
  | APICallRateForCustomerExceeded 
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
  | CacheSubnetGroupAlreadyExists 
  | CacheSubnetGroupInUse 
  | CacheSubnetGroupNotFoundFault 
  | CacheSubnetGroupQuotaExceeded 
  | CacheSubnetQuotaExceededFault 
  | ClusterQuotaForCustomerExceeded 
  | DefaultUserAssociatedToUserGroup 
  | DefaultUserRequired 
  | DryRunOperation 
  | DuplicateUserName 
  | GlobalReplicationGroupAlreadyExistsFault 
  | GlobalReplicationGroupNotFoundFault 
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
  | InvalidGlobalReplicationGroupState 
  | InvalidKMSKeyFault 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | InvalidReplicationGroupState 
  | InvalidSnapshotState 
  | InvalidSubnet 
  | InvalidUserGroupState 
  | InvalidUserState 
  | InvalidVPCNetworkStateFault 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | NoOperationFault 
  | NodeGroupNotFoundFault 
  | NodeGroupsPerReplicationGroupQuotaExceeded 
  | NodeQuotaForClusterExceeded 
  | NodeQuotaForCustomerExceeded 
  | OptInRequired 
  | PendingVerification 
  | QuotaExceeded_CacheSecurityGroup 
  | ReplicationGroupAlreadyExists 
  | ReplicationGroupAlreadyUnderMigrationFault 
  | ReplicationGroupNotFoundFault 
  | ReplicationGroupNotUnderMigrationFault 
  | RequestExpired 
  | RequestLimitExceeded 
  | ReservedCacheNodeAlreadyExists 
  | ReservedCacheNodeNotFound 
  | ReservedCacheNodeQuotaExceeded 
  | ReservedCacheNodesOfferingNotFound 
  | ServiceLinkedRoleNotFoundFault 
  | ServiceUnavailable 
  | ServiceUpdateNotFoundFault 
  | SnapshotAlreadyExistsFault 
  | SnapshotFeatureNotSupportedFault 
  | SnapshotNotFoundFault 
  | SnapshotQuotaExceededFault 
  | SubnetInUse 
  | SubnetNotAllowedFault 
  | TagNotFound 
  | TagQuotaPerResourceExceeded 
  | TestFailoverNotAvailableFault 
  | Throttling 
  | UnauthorizedOperation 
  | UnknownParameter 
  | UnsupportedProtocol 
  | UserAlreadyExists 
  | UserGroupAlreadyExists 
  | UserGroupNotFound 
  | UserGroupQuotaExceeded 
  | UserNotFound 
  | UserQuotaExceeded 
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
  | APICallRateForCustomerExceeded -> Some 400
  | AuthFailure -> None
  | AuthorizationAlreadyExists -> Some 400
  | AuthorizationNotFound -> Some 404
  | Blocked -> None
  | CacheClusterAlreadyExists -> Some 400
  | CacheClusterNotFound -> Some 404
  | CacheParameterGroupAlreadyExists -> Some 400
  | CacheParameterGroupNotFound -> Some 404
  | CacheParameterGroupQuotaExceeded -> Some 400
  | CacheSecurityGroupAlreadyExists -> Some 400
  | CacheSecurityGroupNotFound -> Some 404
  | CacheSubnetGroupAlreadyExists -> Some 400
  | CacheSubnetGroupInUse -> Some 400
  | CacheSubnetGroupNotFoundFault -> Some 400
  | CacheSubnetGroupQuotaExceeded -> Some 400
  | CacheSubnetQuotaExceededFault -> Some 400
  | ClusterQuotaForCustomerExceeded -> Some 400
  | DefaultUserAssociatedToUserGroup -> Some 400
  | DefaultUserRequired -> Some 400
  | DryRunOperation -> None
  | DuplicateUserName -> Some 400
  | GlobalReplicationGroupAlreadyExistsFault -> Some 400
  | GlobalReplicationGroupNotFoundFault -> Some 404
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InsufficientCacheClusterCapacity -> Some 400
  | InternalFailure -> Some 500
  | InvalidARN -> Some 400
  | InvalidAction -> Some 400
  | InvalidCacheClusterState -> Some 400
  | InvalidCacheParameterGroupState -> Some 400
  | InvalidCacheSecurityGroupState -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidGlobalReplicationGroupState -> Some 400
  | InvalidKMSKeyFault -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidReplicationGroupState -> Some 400
  | InvalidSnapshotState -> Some 400
  | InvalidSubnet -> Some 400
  | InvalidUserGroupState -> Some 400
  | InvalidUserState -> Some 400
  | InvalidVPCNetworkStateFault -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NoOperationFault -> Some 400
  | NodeGroupNotFoundFault -> Some 404
  | NodeGroupsPerReplicationGroupQuotaExceeded -> Some 400
  | NodeQuotaForClusterExceeded -> Some 400
  | NodeQuotaForCustomerExceeded -> Some 400
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | QuotaExceeded_CacheSecurityGroup -> Some 400
  | ReplicationGroupAlreadyExists -> Some 400
  | ReplicationGroupAlreadyUnderMigrationFault -> Some 400
  | ReplicationGroupNotFoundFault -> Some 404
  | ReplicationGroupNotUnderMigrationFault -> Some 400
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ReservedCacheNodeAlreadyExists -> Some 404
  | ReservedCacheNodeNotFound -> Some 404
  | ReservedCacheNodeQuotaExceeded -> Some 400
  | ReservedCacheNodesOfferingNotFound -> Some 404
  | ServiceLinkedRoleNotFoundFault -> Some 400
  | ServiceUnavailable -> Some 503
  | ServiceUpdateNotFoundFault -> Some 404
  | SnapshotAlreadyExistsFault -> Some 400
  | SnapshotFeatureNotSupportedFault -> Some 400
  | SnapshotNotFoundFault -> Some 404
  | SnapshotQuotaExceededFault -> Some 400
  | SubnetInUse -> Some 400
  | SubnetNotAllowedFault -> Some 400
  | TagNotFound -> Some 404
  | TagQuotaPerResourceExceeded -> Some 400
  | TestFailoverNotAvailableFault -> Some 400
  | Throttling -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | UserAlreadyExists -> Some 400
  | UserGroupAlreadyExists -> Some 400
  | UserGroupNotFound -> Some 404
  | UserGroupQuotaExceeded -> Some 400
  | UserNotFound -> Some 404
  | UserQuotaExceeded -> Some 400
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | APICallRateForCustomerExceeded -> "APICallRateForCustomerExceeded"
  | AuthFailure -> "AuthFailure"
  | AuthorizationAlreadyExists -> "AuthorizationAlreadyExists"
  | AuthorizationNotFound -> "AuthorizationNotFound"
  | Blocked -> "Blocked"
  | CacheClusterAlreadyExists -> "CacheClusterAlreadyExists"
  | CacheClusterNotFound -> "CacheClusterNotFound"
  | CacheParameterGroupAlreadyExists -> "CacheParameterGroupAlreadyExists"
  | CacheParameterGroupNotFound -> "CacheParameterGroupNotFound"
  | CacheParameterGroupQuotaExceeded -> "CacheParameterGroupQuotaExceeded"
  | CacheSecurityGroupAlreadyExists -> "CacheSecurityGroupAlreadyExists"
  | CacheSecurityGroupNotFound -> "CacheSecurityGroupNotFound"
  | CacheSubnetGroupAlreadyExists -> "CacheSubnetGroupAlreadyExists"
  | CacheSubnetGroupInUse -> "CacheSubnetGroupInUse"
  | CacheSubnetGroupNotFoundFault -> "CacheSubnetGroupNotFoundFault"
  | CacheSubnetGroupQuotaExceeded -> "CacheSubnetGroupQuotaExceeded"
  | CacheSubnetQuotaExceededFault -> "CacheSubnetQuotaExceededFault"
  | ClusterQuotaForCustomerExceeded -> "ClusterQuotaForCustomerExceeded"
  | DefaultUserAssociatedToUserGroup -> "DefaultUserAssociatedToUserGroup"
  | DefaultUserRequired -> "DefaultUserRequired"
  | DryRunOperation -> "DryRunOperation"
  | DuplicateUserName -> "DuplicateUserName"
  | GlobalReplicationGroupAlreadyExistsFault ->
      "GlobalReplicationGroupAlreadyExistsFault"
  | GlobalReplicationGroupNotFoundFault ->
      "GlobalReplicationGroupNotFoundFault"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InsufficientCacheClusterCapacity -> "InsufficientCacheClusterCapacity"
  | InternalFailure -> "InternalFailure"
  | InvalidARN -> "InvalidARN"
  | InvalidAction -> "InvalidAction"
  | InvalidCacheClusterState -> "InvalidCacheClusterState"
  | InvalidCacheParameterGroupState -> "InvalidCacheParameterGroupState"
  | InvalidCacheSecurityGroupState -> "InvalidCacheSecurityGroupState"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidGlobalReplicationGroupState ->
      "InvalidGlobalReplicationGroupState"
  | InvalidKMSKeyFault -> "InvalidKMSKeyFault"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidReplicationGroupState -> "InvalidReplicationGroupState"
  | InvalidSnapshotState -> "InvalidSnapshotState"
  | InvalidSubnet -> "InvalidSubnet"
  | InvalidUserGroupState -> "InvalidUserGroupState"
  | InvalidUserState -> "InvalidUserState"
  | InvalidVPCNetworkStateFault -> "InvalidVPCNetworkStateFault"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NoOperationFault -> "NoOperationFault"
  | NodeGroupNotFoundFault -> "NodeGroupNotFoundFault"
  | NodeGroupsPerReplicationGroupQuotaExceeded ->
      "NodeGroupsPerReplicationGroupQuotaExceeded"
  | NodeQuotaForClusterExceeded -> "NodeQuotaForClusterExceeded"
  | NodeQuotaForCustomerExceeded -> "NodeQuotaForCustomerExceeded"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | QuotaExceeded_CacheSecurityGroup -> "QuotaExceeded.CacheSecurityGroup"
  | ReplicationGroupAlreadyExists -> "ReplicationGroupAlreadyExists"
  | ReplicationGroupAlreadyUnderMigrationFault ->
      "ReplicationGroupAlreadyUnderMigrationFault"
  | ReplicationGroupNotFoundFault -> "ReplicationGroupNotFoundFault"
  | ReplicationGroupNotUnderMigrationFault ->
      "ReplicationGroupNotUnderMigrationFault"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ReservedCacheNodeAlreadyExists -> "ReservedCacheNodeAlreadyExists"
  | ReservedCacheNodeNotFound -> "ReservedCacheNodeNotFound"
  | ReservedCacheNodeQuotaExceeded -> "ReservedCacheNodeQuotaExceeded"
  | ReservedCacheNodesOfferingNotFound ->
      "ReservedCacheNodesOfferingNotFound"
  | ServiceLinkedRoleNotFoundFault -> "ServiceLinkedRoleNotFoundFault"
  | ServiceUnavailable -> "ServiceUnavailable"
  | ServiceUpdateNotFoundFault -> "ServiceUpdateNotFoundFault"
  | SnapshotAlreadyExistsFault -> "SnapshotAlreadyExistsFault"
  | SnapshotFeatureNotSupportedFault -> "SnapshotFeatureNotSupportedFault"
  | SnapshotNotFoundFault -> "SnapshotNotFoundFault"
  | SnapshotQuotaExceededFault -> "SnapshotQuotaExceededFault"
  | SubnetInUse -> "SubnetInUse"
  | SubnetNotAllowedFault -> "SubnetNotAllowedFault"
  | TagNotFound -> "TagNotFound"
  | TagQuotaPerResourceExceeded -> "TagQuotaPerResourceExceeded"
  | TestFailoverNotAvailableFault -> "TestFailoverNotAvailableFault"
  | Throttling -> "Throttling"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | UserAlreadyExists -> "UserAlreadyExists"
  | UserGroupAlreadyExists -> "UserGroupAlreadyExists"
  | UserGroupNotFound -> "UserGroupNotFound"
  | UserGroupQuotaExceeded -> "UserGroupQuotaExceeded"
  | UserNotFound -> "UserNotFound"
  | UserQuotaExceeded -> "UserQuotaExceeded"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "APICallRateForCustomerExceeded" -> Some APICallRateForCustomerExceeded
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
  | "CacheSubnetGroupAlreadyExists" -> Some CacheSubnetGroupAlreadyExists
  | "CacheSubnetGroupInUse" -> Some CacheSubnetGroupInUse
  | "CacheSubnetGroupNotFoundFault" -> Some CacheSubnetGroupNotFoundFault
  | "CacheSubnetGroupQuotaExceeded" -> Some CacheSubnetGroupQuotaExceeded
  | "CacheSubnetQuotaExceededFault" -> Some CacheSubnetQuotaExceededFault
  | "ClusterQuotaForCustomerExceeded" -> Some ClusterQuotaForCustomerExceeded
  | "DefaultUserAssociatedToUserGroup" ->
      Some DefaultUserAssociatedToUserGroup
  | "DefaultUserRequired" -> Some DefaultUserRequired
  | "DryRunOperation" -> Some DryRunOperation
  | "DuplicateUserName" -> Some DuplicateUserName
  | "GlobalReplicationGroupAlreadyExistsFault" ->
      Some GlobalReplicationGroupAlreadyExistsFault
  | "GlobalReplicationGroupNotFoundFault" ->
      Some GlobalReplicationGroupNotFoundFault
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
  | "InvalidGlobalReplicationGroupState" ->
      Some InvalidGlobalReplicationGroupState
  | "InvalidKMSKeyFault" -> Some InvalidKMSKeyFault
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidReplicationGroupState" -> Some InvalidReplicationGroupState
  | "InvalidSnapshotState" -> Some InvalidSnapshotState
  | "InvalidSubnet" -> Some InvalidSubnet
  | "InvalidUserGroupState" -> Some InvalidUserGroupState
  | "InvalidUserState" -> Some InvalidUserState
  | "InvalidVPCNetworkStateFault" -> Some InvalidVPCNetworkStateFault
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NoOperationFault" -> Some NoOperationFault
  | "NodeGroupNotFoundFault" -> Some NodeGroupNotFoundFault
  | "NodeGroupsPerReplicationGroupQuotaExceeded" ->
      Some NodeGroupsPerReplicationGroupQuotaExceeded
  | "NodeQuotaForClusterExceeded" -> Some NodeQuotaForClusterExceeded
  | "NodeQuotaForCustomerExceeded" -> Some NodeQuotaForCustomerExceeded
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "QuotaExceeded.CacheSecurityGroup" ->
      Some QuotaExceeded_CacheSecurityGroup
  | "ReplicationGroupAlreadyExists" -> Some ReplicationGroupAlreadyExists
  | "ReplicationGroupAlreadyUnderMigrationFault" ->
      Some ReplicationGroupAlreadyUnderMigrationFault
  | "ReplicationGroupNotFoundFault" -> Some ReplicationGroupNotFoundFault
  | "ReplicationGroupNotUnderMigrationFault" ->
      Some ReplicationGroupNotUnderMigrationFault
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ReservedCacheNodeAlreadyExists" -> Some ReservedCacheNodeAlreadyExists
  | "ReservedCacheNodeNotFound" -> Some ReservedCacheNodeNotFound
  | "ReservedCacheNodeQuotaExceeded" -> Some ReservedCacheNodeQuotaExceeded
  | "ReservedCacheNodesOfferingNotFound" ->
      Some ReservedCacheNodesOfferingNotFound
  | "ServiceLinkedRoleNotFoundFault" -> Some ServiceLinkedRoleNotFoundFault
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "ServiceUpdateNotFoundFault" -> Some ServiceUpdateNotFoundFault
  | "SnapshotAlreadyExistsFault" -> Some SnapshotAlreadyExistsFault
  | "SnapshotFeatureNotSupportedFault" ->
      Some SnapshotFeatureNotSupportedFault
  | "SnapshotNotFoundFault" -> Some SnapshotNotFoundFault
  | "SnapshotQuotaExceededFault" -> Some SnapshotQuotaExceededFault
  | "SubnetInUse" -> Some SubnetInUse
  | "SubnetNotAllowedFault" -> Some SubnetNotAllowedFault
  | "TagNotFound" -> Some TagNotFound
  | "TagQuotaPerResourceExceeded" -> Some TagQuotaPerResourceExceeded
  | "TestFailoverNotAvailableFault" -> Some TestFailoverNotAvailableFault
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "UserAlreadyExists" -> Some UserAlreadyExists
  | "UserGroupAlreadyExists" -> Some UserGroupAlreadyExists
  | "UserGroupNotFound" -> Some UserGroupNotFound
  | "UserGroupQuotaExceeded" -> Some UserGroupQuotaExceeded
  | "UserNotFound" -> Some UserNotFound
  | "UserQuotaExceeded" -> Some UserQuotaExceeded
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None