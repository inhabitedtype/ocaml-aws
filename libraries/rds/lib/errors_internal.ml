type t =
  | AuthFailure 
  | AuthorizationAlreadyExists 
  | AuthorizationNotFound 
  | AuthorizationQuotaExceeded 
  | BackupPolicyNotFoundFault 
  | Blocked 
  | CertificateNotFound 
  | CustomAvailabilityZoneAlreadyExists 
  | CustomAvailabilityZoneNotFound 
  | CustomAvailabilityZoneQuotaExceeded 
  | DBClusterAlreadyExistsFault 
  | DBClusterBacktrackNotFoundFault 
  | DBClusterEndpointAlreadyExistsFault 
  | DBClusterEndpointNotFoundFault 
  | DBClusterEndpointQuotaExceededFault 
  | DBClusterNotFoundFault 
  | DBClusterParameterGroupNotFound 
  | DBClusterQuotaExceededFault 
  | DBClusterRoleAlreadyExists 
  | DBClusterRoleNotFound 
  | DBClusterRoleQuotaExceeded 
  | DBClusterSnapshotAlreadyExistsFault 
  | DBClusterSnapshotNotFoundFault 
  | DBInstanceAlreadyExists 
  | DBInstanceAutomatedBackupNotFound 
  | DBInstanceAutomatedBackupQuotaExceeded 
  | DBInstanceNotFound 
  | DBInstanceRoleAlreadyExists 
  | DBInstanceRoleNotFound 
  | DBInstanceRoleQuotaExceeded 
  | DBLogFileNotFoundFault 
  | DBParameterGroupAlreadyExists 
  | DBParameterGroupNotFound 
  | DBParameterGroupQuotaExceeded 
  | DBProxyNotFoundFault 
  | DBProxyQuotaExceededFault 
  | DBProxyTargetAlreadyRegisteredFault 
  | DBProxyTargetExistsFault 
  | DBProxyTargetGroupNotFoundFault 
  | DBProxyTargetNotFoundFault 
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
  | ExportTaskAlreadyExists 
  | ExportTaskNotFound 
  | GlobalClusterAlreadyExistsFault 
  | GlobalClusterNotFoundFault 
  | GlobalClusterQuotaExceededFault 
  | IamRoleMissingPermissions 
  | IamRoleNotFound 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InstallationMediaAlreadyExists 
  | InstallationMediaNotFound 
  | InstanceQuotaExceeded 
  | InsufficientAvailableIPsInSubnetFault 
  | InsufficientDBClusterCapacityFault 
  | InsufficientDBInstanceCapacity 
  | InsufficientStorageClusterCapacity 
  | InternalFailure 
  | InvalidAction 
  | InvalidClientTokenId 
  | InvalidDBClusterCapacityFault 
  | InvalidDBClusterEndpointStateFault 
  | InvalidDBClusterSnapshotStateFault 
  | InvalidDBClusterStateFault 
  | InvalidDBInstanceAutomatedBackupState 
  | InvalidDBInstanceState 
  | InvalidDBParameterGroupState 
  | InvalidDBProxyStateFault 
  | InvalidDBSecurityGroupState 
  | InvalidDBSnapshotState 
  | InvalidDBSubnetGroupFault 
  | InvalidDBSubnetGroupStateFault 
  | InvalidDBSubnetStateFault 
  | InvalidEventSubscriptionState 
  | InvalidExportOnly 
  | InvalidExportSourceState 
  | InvalidExportTaskStateFault 
  | InvalidGlobalClusterStateFault 
  | InvalidOptionGroupStateFault 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | InvalidRestoreFault 
  | InvalidS3BucketFault 
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
  | SharedSnapshotQuotaExceeded 
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
  | BackupPolicyNotFoundFault -> Some 404
  | Blocked -> None
  | CertificateNotFound -> Some 404
  | CustomAvailabilityZoneAlreadyExists -> Some 400
  | CustomAvailabilityZoneNotFound -> Some 404
  | CustomAvailabilityZoneQuotaExceeded -> Some 400
  | DBClusterAlreadyExistsFault -> Some 400
  | DBClusterBacktrackNotFoundFault -> Some 404
  | DBClusterEndpointAlreadyExistsFault -> Some 400
  | DBClusterEndpointNotFoundFault -> Some 400
  | DBClusterEndpointQuotaExceededFault -> Some 403
  | DBClusterNotFoundFault -> Some 404
  | DBClusterParameterGroupNotFound -> Some 404
  | DBClusterQuotaExceededFault -> Some 403
  | DBClusterRoleAlreadyExists -> Some 400
  | DBClusterRoleNotFound -> Some 404
  | DBClusterRoleQuotaExceeded -> Some 400
  | DBClusterSnapshotAlreadyExistsFault -> Some 400
  | DBClusterSnapshotNotFoundFault -> Some 404
  | DBInstanceAlreadyExists -> Some 400
  | DBInstanceAutomatedBackupNotFound -> Some 404
  | DBInstanceAutomatedBackupQuotaExceeded -> Some 400
  | DBInstanceNotFound -> Some 404
  | DBInstanceRoleAlreadyExists -> Some 400
  | DBInstanceRoleNotFound -> Some 404
  | DBInstanceRoleQuotaExceeded -> Some 400
  | DBLogFileNotFoundFault -> Some 404
  | DBParameterGroupAlreadyExists -> Some 400
  | DBParameterGroupNotFound -> Some 404
  | DBParameterGroupQuotaExceeded -> Some 400
  | DBProxyNotFoundFault -> Some 404
  | DBProxyQuotaExceededFault -> Some 400
  | DBProxyTargetAlreadyRegisteredFault -> Some 400
  | DBProxyTargetExistsFault -> Some 400
  | DBProxyTargetGroupNotFoundFault -> Some 404
  | DBProxyTargetNotFoundFault -> Some 404
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
  | ExportTaskAlreadyExists -> Some 400
  | ExportTaskNotFound -> Some 404
  | GlobalClusterAlreadyExistsFault -> Some 400
  | GlobalClusterNotFoundFault -> Some 404
  | GlobalClusterQuotaExceededFault -> Some 400
  | IamRoleMissingPermissions -> Some 400
  | IamRoleNotFound -> Some 404
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InstallationMediaAlreadyExists -> Some 400
  | InstallationMediaNotFound -> Some 404
  | InstanceQuotaExceeded -> Some 400
  | InsufficientAvailableIPsInSubnetFault -> Some 400
  | InsufficientDBClusterCapacityFault -> Some 403
  | InsufficientDBInstanceCapacity -> Some 400
  | InsufficientStorageClusterCapacity -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidDBClusterCapacityFault -> Some 400
  | InvalidDBClusterEndpointStateFault -> Some 400
  | InvalidDBClusterSnapshotStateFault -> Some 400
  | InvalidDBClusterStateFault -> Some 400
  | InvalidDBInstanceAutomatedBackupState -> Some 400
  | InvalidDBInstanceState -> Some 400
  | InvalidDBParameterGroupState -> Some 400
  | InvalidDBProxyStateFault -> Some 400
  | InvalidDBSecurityGroupState -> Some 400
  | InvalidDBSnapshotState -> Some 400
  | InvalidDBSubnetGroupFault -> Some 400
  | InvalidDBSubnetGroupStateFault -> Some 400
  | InvalidDBSubnetStateFault -> Some 400
  | InvalidEventSubscriptionState -> Some 400
  | InvalidExportOnly -> Some 400
  | InvalidExportSourceState -> Some 400
  | InvalidExportTaskStateFault -> Some 400
  | InvalidGlobalClusterStateFault -> Some 400
  | InvalidOptionGroupStateFault -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidRestoreFault -> Some 400
  | InvalidS3BucketFault -> Some 400
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
  | SharedSnapshotQuotaExceeded -> Some 400
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
  | BackupPolicyNotFoundFault -> "BackupPolicyNotFoundFault"
  | Blocked -> "Blocked"
  | CertificateNotFound -> "CertificateNotFound"
  | CustomAvailabilityZoneAlreadyExists ->
      "CustomAvailabilityZoneAlreadyExists"
  | CustomAvailabilityZoneNotFound -> "CustomAvailabilityZoneNotFound"
  | CustomAvailabilityZoneQuotaExceeded ->
      "CustomAvailabilityZoneQuotaExceeded"
  | DBClusterAlreadyExistsFault -> "DBClusterAlreadyExistsFault"
  | DBClusterBacktrackNotFoundFault -> "DBClusterBacktrackNotFoundFault"
  | DBClusterEndpointAlreadyExistsFault ->
      "DBClusterEndpointAlreadyExistsFault"
  | DBClusterEndpointNotFoundFault -> "DBClusterEndpointNotFoundFault"
  | DBClusterEndpointQuotaExceededFault ->
      "DBClusterEndpointQuotaExceededFault"
  | DBClusterNotFoundFault -> "DBClusterNotFoundFault"
  | DBClusterParameterGroupNotFound -> "DBClusterParameterGroupNotFound"
  | DBClusterQuotaExceededFault -> "DBClusterQuotaExceededFault"
  | DBClusterRoleAlreadyExists -> "DBClusterRoleAlreadyExists"
  | DBClusterRoleNotFound -> "DBClusterRoleNotFound"
  | DBClusterRoleQuotaExceeded -> "DBClusterRoleQuotaExceeded"
  | DBClusterSnapshotAlreadyExistsFault ->
      "DBClusterSnapshotAlreadyExistsFault"
  | DBClusterSnapshotNotFoundFault -> "DBClusterSnapshotNotFoundFault"
  | DBInstanceAlreadyExists -> "DBInstanceAlreadyExists"
  | DBInstanceAutomatedBackupNotFound -> "DBInstanceAutomatedBackupNotFound"
  | DBInstanceAutomatedBackupQuotaExceeded ->
      "DBInstanceAutomatedBackupQuotaExceeded"
  | DBInstanceNotFound -> "DBInstanceNotFound"
  | DBInstanceRoleAlreadyExists -> "DBInstanceRoleAlreadyExists"
  | DBInstanceRoleNotFound -> "DBInstanceRoleNotFound"
  | DBInstanceRoleQuotaExceeded -> "DBInstanceRoleQuotaExceeded"
  | DBLogFileNotFoundFault -> "DBLogFileNotFoundFault"
  | DBParameterGroupAlreadyExists -> "DBParameterGroupAlreadyExists"
  | DBParameterGroupNotFound -> "DBParameterGroupNotFound"
  | DBParameterGroupQuotaExceeded -> "DBParameterGroupQuotaExceeded"
  | DBProxyNotFoundFault -> "DBProxyNotFoundFault"
  | DBProxyQuotaExceededFault -> "DBProxyQuotaExceededFault"
  | DBProxyTargetAlreadyRegisteredFault ->
      "DBProxyTargetAlreadyRegisteredFault"
  | DBProxyTargetExistsFault -> "DBProxyTargetExistsFault"
  | DBProxyTargetGroupNotFoundFault -> "DBProxyTargetGroupNotFoundFault"
  | DBProxyTargetNotFoundFault -> "DBProxyTargetNotFoundFault"
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
  | ExportTaskAlreadyExists -> "ExportTaskAlreadyExists"
  | ExportTaskNotFound -> "ExportTaskNotFound"
  | GlobalClusterAlreadyExistsFault -> "GlobalClusterAlreadyExistsFault"
  | GlobalClusterNotFoundFault -> "GlobalClusterNotFoundFault"
  | GlobalClusterQuotaExceededFault -> "GlobalClusterQuotaExceededFault"
  | IamRoleMissingPermissions -> "IamRoleMissingPermissions"
  | IamRoleNotFound -> "IamRoleNotFound"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InstallationMediaAlreadyExists -> "InstallationMediaAlreadyExists"
  | InstallationMediaNotFound -> "InstallationMediaNotFound"
  | InstanceQuotaExceeded -> "InstanceQuotaExceeded"
  | InsufficientAvailableIPsInSubnetFault ->
      "InsufficientAvailableIPsInSubnetFault"
  | InsufficientDBClusterCapacityFault ->
      "InsufficientDBClusterCapacityFault"
  | InsufficientDBInstanceCapacity -> "InsufficientDBInstanceCapacity"
  | InsufficientStorageClusterCapacity ->
      "InsufficientStorageClusterCapacity"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidDBClusterCapacityFault -> "InvalidDBClusterCapacityFault"
  | InvalidDBClusterEndpointStateFault ->
      "InvalidDBClusterEndpointStateFault"
  | InvalidDBClusterSnapshotStateFault ->
      "InvalidDBClusterSnapshotStateFault"
  | InvalidDBClusterStateFault -> "InvalidDBClusterStateFault"
  | InvalidDBInstanceAutomatedBackupState ->
      "InvalidDBInstanceAutomatedBackupState"
  | InvalidDBInstanceState -> "InvalidDBInstanceState"
  | InvalidDBParameterGroupState -> "InvalidDBParameterGroupState"
  | InvalidDBProxyStateFault -> "InvalidDBProxyStateFault"
  | InvalidDBSecurityGroupState -> "InvalidDBSecurityGroupState"
  | InvalidDBSnapshotState -> "InvalidDBSnapshotState"
  | InvalidDBSubnetGroupFault -> "InvalidDBSubnetGroupFault"
  | InvalidDBSubnetGroupStateFault -> "InvalidDBSubnetGroupStateFault"
  | InvalidDBSubnetStateFault -> "InvalidDBSubnetStateFault"
  | InvalidEventSubscriptionState -> "InvalidEventSubscriptionState"
  | InvalidExportOnly -> "InvalidExportOnly"
  | InvalidExportSourceState -> "InvalidExportSourceState"
  | InvalidExportTaskStateFault -> "InvalidExportTaskStateFault"
  | InvalidGlobalClusterStateFault -> "InvalidGlobalClusterStateFault"
  | InvalidOptionGroupStateFault -> "InvalidOptionGroupStateFault"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidRestoreFault -> "InvalidRestoreFault"
  | InvalidS3BucketFault -> "InvalidS3BucketFault"
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
  | SharedSnapshotQuotaExceeded -> "SharedSnapshotQuotaExceeded"
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
  | "BackupPolicyNotFoundFault" -> Some BackupPolicyNotFoundFault
  | "Blocked" -> Some Blocked
  | "CertificateNotFound" -> Some CertificateNotFound
  | "CustomAvailabilityZoneAlreadyExists" ->
      Some CustomAvailabilityZoneAlreadyExists
  | "CustomAvailabilityZoneNotFound" -> Some CustomAvailabilityZoneNotFound
  | "CustomAvailabilityZoneQuotaExceeded" ->
      Some CustomAvailabilityZoneQuotaExceeded
  | "DBClusterAlreadyExistsFault" -> Some DBClusterAlreadyExistsFault
  | "DBClusterBacktrackNotFoundFault" -> Some DBClusterBacktrackNotFoundFault
  | "DBClusterEndpointAlreadyExistsFault" ->
      Some DBClusterEndpointAlreadyExistsFault
  | "DBClusterEndpointNotFoundFault" -> Some DBClusterEndpointNotFoundFault
  | "DBClusterEndpointQuotaExceededFault" ->
      Some DBClusterEndpointQuotaExceededFault
  | "DBClusterNotFoundFault" -> Some DBClusterNotFoundFault
  | "DBClusterParameterGroupNotFound" -> Some DBClusterParameterGroupNotFound
  | "DBClusterQuotaExceededFault" -> Some DBClusterQuotaExceededFault
  | "DBClusterRoleAlreadyExists" -> Some DBClusterRoleAlreadyExists
  | "DBClusterRoleNotFound" -> Some DBClusterRoleNotFound
  | "DBClusterRoleQuotaExceeded" -> Some DBClusterRoleQuotaExceeded
  | "DBClusterSnapshotAlreadyExistsFault" ->
      Some DBClusterSnapshotAlreadyExistsFault
  | "DBClusterSnapshotNotFoundFault" -> Some DBClusterSnapshotNotFoundFault
  | "DBInstanceAlreadyExists" -> Some DBInstanceAlreadyExists
  | "DBInstanceAutomatedBackupNotFound" ->
      Some DBInstanceAutomatedBackupNotFound
  | "DBInstanceAutomatedBackupQuotaExceeded" ->
      Some DBInstanceAutomatedBackupQuotaExceeded
  | "DBInstanceNotFound" -> Some DBInstanceNotFound
  | "DBInstanceRoleAlreadyExists" -> Some DBInstanceRoleAlreadyExists
  | "DBInstanceRoleNotFound" -> Some DBInstanceRoleNotFound
  | "DBInstanceRoleQuotaExceeded" -> Some DBInstanceRoleQuotaExceeded
  | "DBLogFileNotFoundFault" -> Some DBLogFileNotFoundFault
  | "DBParameterGroupAlreadyExists" -> Some DBParameterGroupAlreadyExists
  | "DBParameterGroupNotFound" -> Some DBParameterGroupNotFound
  | "DBParameterGroupQuotaExceeded" -> Some DBParameterGroupQuotaExceeded
  | "DBProxyNotFoundFault" -> Some DBProxyNotFoundFault
  | "DBProxyQuotaExceededFault" -> Some DBProxyQuotaExceededFault
  | "DBProxyTargetAlreadyRegisteredFault" ->
      Some DBProxyTargetAlreadyRegisteredFault
  | "DBProxyTargetExistsFault" -> Some DBProxyTargetExistsFault
  | "DBProxyTargetGroupNotFoundFault" -> Some DBProxyTargetGroupNotFoundFault
  | "DBProxyTargetNotFoundFault" -> Some DBProxyTargetNotFoundFault
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
  | "ExportTaskAlreadyExists" -> Some ExportTaskAlreadyExists
  | "ExportTaskNotFound" -> Some ExportTaskNotFound
  | "GlobalClusterAlreadyExistsFault" -> Some GlobalClusterAlreadyExistsFault
  | "GlobalClusterNotFoundFault" -> Some GlobalClusterNotFoundFault
  | "GlobalClusterQuotaExceededFault" -> Some GlobalClusterQuotaExceededFault
  | "IamRoleMissingPermissions" -> Some IamRoleMissingPermissions
  | "IamRoleNotFound" -> Some IamRoleNotFound
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InstallationMediaAlreadyExists" -> Some InstallationMediaAlreadyExists
  | "InstallationMediaNotFound" -> Some InstallationMediaNotFound
  | "InstanceQuotaExceeded" -> Some InstanceQuotaExceeded
  | "InsufficientAvailableIPsInSubnetFault" ->
      Some InsufficientAvailableIPsInSubnetFault
  | "InsufficientDBClusterCapacityFault" ->
      Some InsufficientDBClusterCapacityFault
  | "InsufficientDBInstanceCapacity" -> Some InsufficientDBInstanceCapacity
  | "InsufficientStorageClusterCapacity" ->
      Some InsufficientStorageClusterCapacity
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidDBClusterCapacityFault" -> Some InvalidDBClusterCapacityFault
  | "InvalidDBClusterEndpointStateFault" ->
      Some InvalidDBClusterEndpointStateFault
  | "InvalidDBClusterSnapshotStateFault" ->
      Some InvalidDBClusterSnapshotStateFault
  | "InvalidDBClusterStateFault" -> Some InvalidDBClusterStateFault
  | "InvalidDBInstanceAutomatedBackupState" ->
      Some InvalidDBInstanceAutomatedBackupState
  | "InvalidDBInstanceState" -> Some InvalidDBInstanceState
  | "InvalidDBParameterGroupState" -> Some InvalidDBParameterGroupState
  | "InvalidDBProxyStateFault" -> Some InvalidDBProxyStateFault
  | "InvalidDBSecurityGroupState" -> Some InvalidDBSecurityGroupState
  | "InvalidDBSnapshotState" -> Some InvalidDBSnapshotState
  | "InvalidDBSubnetGroupFault" -> Some InvalidDBSubnetGroupFault
  | "InvalidDBSubnetGroupStateFault" -> Some InvalidDBSubnetGroupStateFault
  | "InvalidDBSubnetStateFault" -> Some InvalidDBSubnetStateFault
  | "InvalidEventSubscriptionState" -> Some InvalidEventSubscriptionState
  | "InvalidExportOnly" -> Some InvalidExportOnly
  | "InvalidExportSourceState" -> Some InvalidExportSourceState
  | "InvalidExportTaskStateFault" -> Some InvalidExportTaskStateFault
  | "InvalidGlobalClusterStateFault" -> Some InvalidGlobalClusterStateFault
  | "InvalidOptionGroupStateFault" -> Some InvalidOptionGroupStateFault
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidRestoreFault" -> Some InvalidRestoreFault
  | "InvalidS3BucketFault" -> Some InvalidS3BucketFault
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
  | "SharedSnapshotQuotaExceeded" -> Some SharedSnapshotQuotaExceeded
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