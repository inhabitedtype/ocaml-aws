type t =
  | AuthFailure 
  | AuthorizationAlreadyExists 
  | AuthorizationNotFound 
  | AuthorizationQuotaExceeded 
  | Blocked 
  | CertificateNotFound 
  | DBClusterAlreadyExistsFault 
  | DBClusterNotFoundFault 
  | DBClusterParameterGroupNotFound 
  | DBClusterQuotaExceededFault 
  | DBClusterSnapshotAlreadyExistsFault 
  | DBClusterSnapshotNotFoundFault 
  | DBInstanceAlreadyExists 
  | DBInstanceNotFound 
  | DBLogFileNotFoundFault 
  | DBParameterGroupAlreadyExists 
  | DBParameterGroupNotFound 
  | DBParameterGroupQuotaExceeded 
  | DBSecurityGroupAlreadyExists 
  | DBSecurityGroupNotFound 
  | DBSecurityGroupNotSupported 
  | DBSnapshotAlreadyExists 
  | DBSnapshotNotFound 
  | DBSubnetGroupAlreadyExists 
  | DBSubnetGroupDoesNotCoverEnoughAZs 
  | DBSubnetGroupNotAllowedFault 
  | DBSubnetGroupNotFoundFault 
  | DBSubnetGroupQuotaExceeded 
  | DBSubnetQuotaExceededFault 
  | DBUpgradeDependencyFailure 
  | DomainNotFoundFault 
  | DryRunOperation 
  | EventSubscriptionQuotaExceeded 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InstanceQuotaExceeded 
  | InsufficientDBClusterCapacityFault 
  | InsufficientDBInstanceCapacity 
  | InsufficientDomainCapacityFault 
  | InsufficientStorageClusterCapacity 
  | InternalFailure 
  | InvalidAction 
  | InvalidClientTokenId 
  | InvalidDBClusterSnapshotStateFault 
  | InvalidDBClusterStateFault 
  | InvalidDBInstanceState 
  | InvalidDBParameterGroupState 
  | InvalidDBSecurityGroupState 
  | InvalidDBSnapshotState 
  | InvalidDBSubnetGroupFault 
  | InvalidDBSubnetGroupStateFault 
  | InvalidDBSubnetStateFault 
  | InvalidEventSubscriptionState 
  | InvalidOptionGroupStateFault 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | InvalidRestoreFault 
  | InvalidSubnet 
  | InvalidVPCNetworkStateFault 
  | KMSKeyNotAccessibleFault 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | OptInRequired 
  | OptionGroupAlreadyExistsFault 
  | OptionGroupNotFoundFault 
  | OptionGroupQuotaExceededFault 
  | PendingVerification 
  | PointInTimeRestoreNotEnabled 
  | ProvisionedIopsNotAvailableInAZFault 
  | QuotaExceeded_DBSecurityGroup 
  | RequestExpired 
  | RequestLimitExceeded 
  | ReservedDBInstanceAlreadyExists 
  | ReservedDBInstanceNotFound 
  | ReservedDBInstanceQuotaExceeded 
  | ReservedDBInstancesOfferingNotFound 
  | ResourceNotFoundFault 
  | SNSInvalidTopic 
  | SNSNoAuthorization 
  | SNSTopicArnNotFound 
  | ServiceUnavailable 
  | SnapshotQuotaExceeded 
  | SourceNotFound 
  | StorageQuotaExceeded 
  | StorageTypeNotSupported 
  | SubnetAlreadyInUse 
  | SubscriptionAlreadyExist 
  | SubscriptionCategoryNotFound 
  | SubscriptionNotFound 
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
  | AuthFailure -> None
  | AuthorizationAlreadyExists -> Some 400
  | AuthorizationNotFound -> Some 404
  | AuthorizationQuotaExceeded -> Some 400
  | Blocked -> None
  | CertificateNotFound -> Some 404
  | DBClusterAlreadyExistsFault -> Some 400
  | DBClusterNotFoundFault -> Some 404
  | DBClusterParameterGroupNotFound -> Some 404
  | DBClusterQuotaExceededFault -> Some 403
  | DBClusterSnapshotAlreadyExistsFault -> Some 400
  | DBClusterSnapshotNotFoundFault -> Some 404
  | DBInstanceAlreadyExists -> Some 400
  | DBInstanceNotFound -> Some 404
  | DBLogFileNotFoundFault -> Some 404
  | DBParameterGroupAlreadyExists -> Some 400
  | DBParameterGroupNotFound -> Some 404
  | DBParameterGroupQuotaExceeded -> Some 400
  | DBSecurityGroupAlreadyExists -> Some 400
  | DBSecurityGroupNotFound -> Some 404
  | DBSecurityGroupNotSupported -> Some 400
  | DBSnapshotAlreadyExists -> Some 400
  | DBSnapshotNotFound -> Some 404
  | DBSubnetGroupAlreadyExists -> Some 400
  | DBSubnetGroupDoesNotCoverEnoughAZs -> Some 400
  | DBSubnetGroupNotAllowedFault -> Some 400
  | DBSubnetGroupNotFoundFault -> Some 404
  | DBSubnetGroupQuotaExceeded -> Some 400
  | DBSubnetQuotaExceededFault -> Some 400
  | DBUpgradeDependencyFailure -> Some 400
  | DomainNotFoundFault -> Some 404
  | DryRunOperation -> None
  | EventSubscriptionQuotaExceeded -> Some 400
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InstanceQuotaExceeded -> Some 400
  | InsufficientDBClusterCapacityFault -> Some 403
  | InsufficientDBInstanceCapacity -> Some 400
  | InsufficientDomainCapacityFault -> Some 400
  | InsufficientStorageClusterCapacity -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidDBClusterSnapshotStateFault -> Some 400
  | InvalidDBClusterStateFault -> Some 400
  | InvalidDBInstanceState -> Some 400
  | InvalidDBParameterGroupState -> Some 400
  | InvalidDBSecurityGroupState -> Some 400
  | InvalidDBSnapshotState -> Some 400
  | InvalidDBSubnetGroupFault -> Some 400
  | InvalidDBSubnetGroupStateFault -> Some 400
  | InvalidDBSubnetStateFault -> Some 400
  | InvalidEventSubscriptionState -> Some 400
  | InvalidOptionGroupStateFault -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidRestoreFault -> Some 400
  | InvalidSubnet -> Some 400
  | InvalidVPCNetworkStateFault -> Some 400
  | KMSKeyNotAccessibleFault -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | OptInRequired -> Some 403
  | OptionGroupAlreadyExistsFault -> Some 400
  | OptionGroupNotFoundFault -> Some 404
  | OptionGroupQuotaExceededFault -> Some 400
  | PendingVerification -> None
  | PointInTimeRestoreNotEnabled -> Some 400
  | ProvisionedIopsNotAvailableInAZFault -> Some 400
  | QuotaExceeded_DBSecurityGroup -> Some 400
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ReservedDBInstanceAlreadyExists -> Some 404
  | ReservedDBInstanceNotFound -> Some 404
  | ReservedDBInstanceQuotaExceeded -> Some 400
  | ReservedDBInstancesOfferingNotFound -> Some 404
  | ResourceNotFoundFault -> Some 404
  | SNSInvalidTopic -> Some 400
  | SNSNoAuthorization -> Some 400
  | SNSTopicArnNotFound -> Some 404
  | ServiceUnavailable -> Some 503
  | SnapshotQuotaExceeded -> Some 400
  | SourceNotFound -> Some 404
  | StorageQuotaExceeded -> Some 400
  | StorageTypeNotSupported -> Some 400
  | SubnetAlreadyInUse -> Some 400
  | SubscriptionAlreadyExist -> Some 400
  | SubscriptionCategoryNotFound -> Some 404
  | SubscriptionNotFound -> Some 404
  | Throttling -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | AuthorizationAlreadyExists -> "AuthorizationAlreadyExists"
  | AuthorizationNotFound -> "AuthorizationNotFound"
  | AuthorizationQuotaExceeded -> "AuthorizationQuotaExceeded"
  | Blocked -> "Blocked"
  | CertificateNotFound -> "CertificateNotFound"
  | DBClusterAlreadyExistsFault -> "DBClusterAlreadyExistsFault"
  | DBClusterNotFoundFault -> "DBClusterNotFoundFault"
  | DBClusterParameterGroupNotFound -> "DBClusterParameterGroupNotFound"
  | DBClusterQuotaExceededFault -> "DBClusterQuotaExceededFault"
  | DBClusterSnapshotAlreadyExistsFault ->
      "DBClusterSnapshotAlreadyExistsFault"
  | DBClusterSnapshotNotFoundFault -> "DBClusterSnapshotNotFoundFault"
  | DBInstanceAlreadyExists -> "DBInstanceAlreadyExists"
  | DBInstanceNotFound -> "DBInstanceNotFound"
  | DBLogFileNotFoundFault -> "DBLogFileNotFoundFault"
  | DBParameterGroupAlreadyExists -> "DBParameterGroupAlreadyExists"
  | DBParameterGroupNotFound -> "DBParameterGroupNotFound"
  | DBParameterGroupQuotaExceeded -> "DBParameterGroupQuotaExceeded"
  | DBSecurityGroupAlreadyExists -> "DBSecurityGroupAlreadyExists"
  | DBSecurityGroupNotFound -> "DBSecurityGroupNotFound"
  | DBSecurityGroupNotSupported -> "DBSecurityGroupNotSupported"
  | DBSnapshotAlreadyExists -> "DBSnapshotAlreadyExists"
  | DBSnapshotNotFound -> "DBSnapshotNotFound"
  | DBSubnetGroupAlreadyExists -> "DBSubnetGroupAlreadyExists"
  | DBSubnetGroupDoesNotCoverEnoughAZs ->
      "DBSubnetGroupDoesNotCoverEnoughAZs"
  | DBSubnetGroupNotAllowedFault -> "DBSubnetGroupNotAllowedFault"
  | DBSubnetGroupNotFoundFault -> "DBSubnetGroupNotFoundFault"
  | DBSubnetGroupQuotaExceeded -> "DBSubnetGroupQuotaExceeded"
  | DBSubnetQuotaExceededFault -> "DBSubnetQuotaExceededFault"
  | DBUpgradeDependencyFailure -> "DBUpgradeDependencyFailure"
  | DomainNotFoundFault -> "DomainNotFoundFault"
  | DryRunOperation -> "DryRunOperation"
  | EventSubscriptionQuotaExceeded -> "EventSubscriptionQuotaExceeded"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InstanceQuotaExceeded -> "InstanceQuotaExceeded"
  | InsufficientDBClusterCapacityFault ->
      "InsufficientDBClusterCapacityFault"
  | InsufficientDBInstanceCapacity -> "InsufficientDBInstanceCapacity"
  | InsufficientDomainCapacityFault -> "InsufficientDomainCapacityFault"
  | InsufficientStorageClusterCapacity ->
      "InsufficientStorageClusterCapacity"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidDBClusterSnapshotStateFault ->
      "InvalidDBClusterSnapshotStateFault"
  | InvalidDBClusterStateFault -> "InvalidDBClusterStateFault"
  | InvalidDBInstanceState -> "InvalidDBInstanceState"
  | InvalidDBParameterGroupState -> "InvalidDBParameterGroupState"
  | InvalidDBSecurityGroupState -> "InvalidDBSecurityGroupState"
  | InvalidDBSnapshotState -> "InvalidDBSnapshotState"
  | InvalidDBSubnetGroupFault -> "InvalidDBSubnetGroupFault"
  | InvalidDBSubnetGroupStateFault -> "InvalidDBSubnetGroupStateFault"
  | InvalidDBSubnetStateFault -> "InvalidDBSubnetStateFault"
  | InvalidEventSubscriptionState -> "InvalidEventSubscriptionState"
  | InvalidOptionGroupStateFault -> "InvalidOptionGroupStateFault"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidRestoreFault -> "InvalidRestoreFault"
  | InvalidSubnet -> "InvalidSubnet"
  | InvalidVPCNetworkStateFault -> "InvalidVPCNetworkStateFault"
  | KMSKeyNotAccessibleFault -> "KMSKeyNotAccessibleFault"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | OptInRequired -> "OptInRequired"
  | OptionGroupAlreadyExistsFault -> "OptionGroupAlreadyExistsFault"
  | OptionGroupNotFoundFault -> "OptionGroupNotFoundFault"
  | OptionGroupQuotaExceededFault -> "OptionGroupQuotaExceededFault"
  | PendingVerification -> "PendingVerification"
  | PointInTimeRestoreNotEnabled -> "PointInTimeRestoreNotEnabled"
  | ProvisionedIopsNotAvailableInAZFault ->
      "ProvisionedIopsNotAvailableInAZFault"
  | QuotaExceeded_DBSecurityGroup -> "QuotaExceeded.DBSecurityGroup"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ReservedDBInstanceAlreadyExists -> "ReservedDBInstanceAlreadyExists"
  | ReservedDBInstanceNotFound -> "ReservedDBInstanceNotFound"
  | ReservedDBInstanceQuotaExceeded -> "ReservedDBInstanceQuotaExceeded"
  | ReservedDBInstancesOfferingNotFound ->
      "ReservedDBInstancesOfferingNotFound"
  | ResourceNotFoundFault -> "ResourceNotFoundFault"
  | SNSInvalidTopic -> "SNSInvalidTopic"
  | SNSNoAuthorization -> "SNSNoAuthorization"
  | SNSTopicArnNotFound -> "SNSTopicArnNotFound"
  | ServiceUnavailable -> "ServiceUnavailable"
  | SnapshotQuotaExceeded -> "SnapshotQuotaExceeded"
  | SourceNotFound -> "SourceNotFound"
  | StorageQuotaExceeded -> "StorageQuotaExceeded"
  | StorageTypeNotSupported -> "StorageTypeNotSupported"
  | SubnetAlreadyInUse -> "SubnetAlreadyInUse"
  | SubscriptionAlreadyExist -> "SubscriptionAlreadyExist"
  | SubscriptionCategoryNotFound -> "SubscriptionCategoryNotFound"
  | SubscriptionNotFound -> "SubscriptionNotFound"
  | Throttling -> "Throttling"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "AuthorizationAlreadyExists" -> Some AuthorizationAlreadyExists
  | "AuthorizationNotFound" -> Some AuthorizationNotFound
  | "AuthorizationQuotaExceeded" -> Some AuthorizationQuotaExceeded
  | "Blocked" -> Some Blocked
  | "CertificateNotFound" -> Some CertificateNotFound
  | "DBClusterAlreadyExistsFault" -> Some DBClusterAlreadyExistsFault
  | "DBClusterNotFoundFault" -> Some DBClusterNotFoundFault
  | "DBClusterParameterGroupNotFound" -> Some DBClusterParameterGroupNotFound
  | "DBClusterQuotaExceededFault" -> Some DBClusterQuotaExceededFault
  | "DBClusterSnapshotAlreadyExistsFault" ->
      Some DBClusterSnapshotAlreadyExistsFault
  | "DBClusterSnapshotNotFoundFault" -> Some DBClusterSnapshotNotFoundFault
  | "DBInstanceAlreadyExists" -> Some DBInstanceAlreadyExists
  | "DBInstanceNotFound" -> Some DBInstanceNotFound
  | "DBLogFileNotFoundFault" -> Some DBLogFileNotFoundFault
  | "DBParameterGroupAlreadyExists" -> Some DBParameterGroupAlreadyExists
  | "DBParameterGroupNotFound" -> Some DBParameterGroupNotFound
  | "DBParameterGroupQuotaExceeded" -> Some DBParameterGroupQuotaExceeded
  | "DBSecurityGroupAlreadyExists" -> Some DBSecurityGroupAlreadyExists
  | "DBSecurityGroupNotFound" -> Some DBSecurityGroupNotFound
  | "DBSecurityGroupNotSupported" -> Some DBSecurityGroupNotSupported
  | "DBSnapshotAlreadyExists" -> Some DBSnapshotAlreadyExists
  | "DBSnapshotNotFound" -> Some DBSnapshotNotFound
  | "DBSubnetGroupAlreadyExists" -> Some DBSubnetGroupAlreadyExists
  | "DBSubnetGroupDoesNotCoverEnoughAZs" ->
      Some DBSubnetGroupDoesNotCoverEnoughAZs
  | "DBSubnetGroupNotAllowedFault" -> Some DBSubnetGroupNotAllowedFault
  | "DBSubnetGroupNotFoundFault" -> Some DBSubnetGroupNotFoundFault
  | "DBSubnetGroupQuotaExceeded" -> Some DBSubnetGroupQuotaExceeded
  | "DBSubnetQuotaExceededFault" -> Some DBSubnetQuotaExceededFault
  | "DBUpgradeDependencyFailure" -> Some DBUpgradeDependencyFailure
  | "DomainNotFoundFault" -> Some DomainNotFoundFault
  | "DryRunOperation" -> Some DryRunOperation
  | "EventSubscriptionQuotaExceeded" -> Some EventSubscriptionQuotaExceeded
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InstanceQuotaExceeded" -> Some InstanceQuotaExceeded
  | "InsufficientDBClusterCapacityFault" ->
      Some InsufficientDBClusterCapacityFault
  | "InsufficientDBInstanceCapacity" -> Some InsufficientDBInstanceCapacity
  | "InsufficientDomainCapacityFault" -> Some InsufficientDomainCapacityFault
  | "InsufficientStorageClusterCapacity" ->
      Some InsufficientStorageClusterCapacity
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidDBClusterSnapshotStateFault" ->
      Some InvalidDBClusterSnapshotStateFault
  | "InvalidDBClusterStateFault" -> Some InvalidDBClusterStateFault
  | "InvalidDBInstanceState" -> Some InvalidDBInstanceState
  | "InvalidDBParameterGroupState" -> Some InvalidDBParameterGroupState
  | "InvalidDBSecurityGroupState" -> Some InvalidDBSecurityGroupState
  | "InvalidDBSnapshotState" -> Some InvalidDBSnapshotState
  | "InvalidDBSubnetGroupFault" -> Some InvalidDBSubnetGroupFault
  | "InvalidDBSubnetGroupStateFault" -> Some InvalidDBSubnetGroupStateFault
  | "InvalidDBSubnetStateFault" -> Some InvalidDBSubnetStateFault
  | "InvalidEventSubscriptionState" -> Some InvalidEventSubscriptionState
  | "InvalidOptionGroupStateFault" -> Some InvalidOptionGroupStateFault
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidRestoreFault" -> Some InvalidRestoreFault
  | "InvalidSubnet" -> Some InvalidSubnet
  | "InvalidVPCNetworkStateFault" -> Some InvalidVPCNetworkStateFault
  | "KMSKeyNotAccessibleFault" -> Some KMSKeyNotAccessibleFault
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "OptInRequired" -> Some OptInRequired
  | "OptionGroupAlreadyExistsFault" -> Some OptionGroupAlreadyExistsFault
  | "OptionGroupNotFoundFault" -> Some OptionGroupNotFoundFault
  | "OptionGroupQuotaExceededFault" -> Some OptionGroupQuotaExceededFault
  | "PendingVerification" -> Some PendingVerification
  | "PointInTimeRestoreNotEnabled" -> Some PointInTimeRestoreNotEnabled
  | "ProvisionedIopsNotAvailableInAZFault" ->
      Some ProvisionedIopsNotAvailableInAZFault
  | "QuotaExceeded.DBSecurityGroup" -> Some QuotaExceeded_DBSecurityGroup
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ReservedDBInstanceAlreadyExists" -> Some ReservedDBInstanceAlreadyExists
  | "ReservedDBInstanceNotFound" -> Some ReservedDBInstanceNotFound
  | "ReservedDBInstanceQuotaExceeded" -> Some ReservedDBInstanceQuotaExceeded
  | "ReservedDBInstancesOfferingNotFound" ->
      Some ReservedDBInstancesOfferingNotFound
  | "ResourceNotFoundFault" -> Some ResourceNotFoundFault
  | "SNSInvalidTopic" -> Some SNSInvalidTopic
  | "SNSNoAuthorization" -> Some SNSNoAuthorization
  | "SNSTopicArnNotFound" -> Some SNSTopicArnNotFound
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "SnapshotQuotaExceeded" -> Some SnapshotQuotaExceeded
  | "SourceNotFound" -> Some SourceNotFound
  | "StorageQuotaExceeded" -> Some StorageQuotaExceeded
  | "StorageTypeNotSupported" -> Some StorageTypeNotSupported
  | "SubnetAlreadyInUse" -> Some SubnetAlreadyInUse
  | "SubscriptionAlreadyExist" -> Some SubscriptionAlreadyExist
  | "SubscriptionCategoryNotFound" -> Some SubscriptionCategoryNotFound
  | "SubscriptionNotFound" -> Some SubscriptionNotFound
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None