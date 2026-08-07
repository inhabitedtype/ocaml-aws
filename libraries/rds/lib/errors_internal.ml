type t =
  | AuthFailure
  | AuthorizationAlreadyExists
  | AuthorizationNotFound
  | AuthorizationQuotaExceeded
  | BackupPolicyNotFoundFault
  | Blocked
  | BlueGreenDeploymentAlreadyExistsFault
  | BlueGreenDeploymentNotFoundFault
  | CertificateNotFound
  | CreateCustomDBEngineVersionFault
  | CustomAvailabilityZoneNotFound
  | CustomDBEngineVersionAlreadyExistsFault
  | CustomDBEngineVersionNotFoundFault
  | CustomDBEngineVersionQuotaExceededFault
  | DBClusterAlreadyExistsFault
  | DBClusterAutomatedBackupNotFoundFault
  | DBClusterAutomatedBackupQuotaExceededFault
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
  | DBInstanceNotReady
  | DBInstanceRoleAlreadyExists
  | DBInstanceRoleNotFound
  | DBInstanceRoleQuotaExceeded
  | DBLogFileNotFoundFault
  | DBParameterGroupAlreadyExists
  | DBParameterGroupNotFound
  | DBParameterGroupQuotaExceeded
  | DBProxyAlreadyExistsFault
  | DBProxyEndpointAlreadyExistsFault
  | DBProxyEndpointNotFoundFault
  | DBProxyEndpointQuotaExceededFault
  | DBProxyNotFoundFault
  | DBProxyQuotaExceededFault
  | DBProxyTargetAlreadyRegisteredFault
  | DBProxyTargetGroupNotFoundFault
  | DBProxyTargetNotFoundFault
  | DBSecurityGroupAlreadyExists
  | DBSecurityGroupNotFound
  | DBSecurityGroupNotSupported
  | DBShardGroupAlreadyExists
  | DBShardGroupNotFound
  | DBSnapshotAlreadyExists
  | DBSnapshotNotFound
  | DBSnapshotTenantDatabaseNotFoundFault
  | DBSubnetGroupAlreadyExists
  | DBSubnetGroupDoesNotCoverEnoughAZs
  | DBSubnetGroupNotAllowedFault
  | DBSubnetGroupNotFoundFault
  | DBSubnetGroupQuotaExceeded
  | DBSubnetQuotaExceededFault
  | DBUpgradeDependencyFailure
  | DomainNotFoundFault
  | DryRunOperation
  | Ec2ImagePropertiesNotSupportedFault
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
  | InstanceQuotaExceeded
  | InsufficientAvailableIPsInSubnetFault
  | InsufficientDBClusterCapacityFault
  | InsufficientDBInstanceCapacity
  | InsufficientStorageClusterCapacity
  | IntegrationAlreadyExistsFault
  | IntegrationConflictOperationFault
  | IntegrationNotFoundFault
  | IntegrationQuotaExceededFault
  | InternalFailure
  | InvalidAction
  | InvalidBlueGreenDeploymentStateFault
  | InvalidClientTokenId
  | InvalidCustomDBEngineVersionStateFault
  | InvalidDBClusterAutomatedBackupStateFault
  | InvalidDBClusterCapacityFault
  | InvalidDBClusterEndpointStateFault
  | InvalidDBClusterSnapshotStateFault
  | InvalidDBClusterStateFault
  | InvalidDBInstanceAutomatedBackupState
  | InvalidDBInstanceState
  | InvalidDBParameterGroupState
  | InvalidDBProxyEndpointStateFault
  | InvalidDBProxyStateFault
  | InvalidDBSecurityGroupState
  | InvalidDBShardGroupState
  | InvalidDBSnapshotState
  | InvalidDBSubnetGroupFault
  | InvalidDBSubnetGroupStateFault
  | InvalidDBSubnetStateFault
  | InvalidEventSubscriptionState
  | InvalidExportOnly
  | InvalidExportSourceState
  | InvalidExportTaskStateFault
  | InvalidGlobalClusterStateFault
  | InvalidIntegrationStateFault
  | InvalidOptionGroupStateFault
  | InvalidParameter
  | InvalidParameterCombination
  | InvalidParameterValue
  | InvalidQueryParameter
  | InvalidResourceStateFault
  | InvalidRestoreFault
  | InvalidS3BucketFault
  | InvalidSubnet
  | InvalidVPCNetworkStateFault
  | KMSKeyNotAccessibleFault
  | MalformedQueryString
  | MaxDBShardGroupLimitReached
  | MissingAction
  | MissingAuthenticationToken
  | MissingParameter
  | NetworkTypeNotSupported
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
  | SourceClusterNotSupportedFault
  | SourceDatabaseNotSupportedFault
  | SourceNotFound
  | StorageQuotaExceeded
  | StorageTypeNotAvailableFault
  | StorageTypeNotSupported
  | SubnetAlreadyInUse
  | SubscriptionAlreadyExist
  | SubscriptionCategoryNotFound
  | SubscriptionNotFound
  | TenantDatabaseAlreadyExists
  | TenantDatabaseNotFound
  | TenantDatabaseQuotaExceeded
  | Throttling
  | UnauthorizedOperation
  | UnknownParameter
  | UnsupportedDBEngineVersion
  | UnsupportedProtocol
  | ValidationError
  | VpcEncryptionControlViolationException
  | Uninhabited

let common =
  [ UnsupportedProtocol
  ; UnknownParameter
  ; UnauthorizedOperation
  ; RequestLimitExceeded
  ; PendingVerification
  ; InvalidParameter
  ; IdempotentParameterMismatch
  ; DryRunOperation
  ; Blocked
  ; AuthFailure
  ; ValidationError
  ; Throttling
  ; ServiceUnavailable
  ; RequestExpired
  ; OptInRequired
  ; MissingParameter
  ; MissingAuthenticationToken
  ; MissingAction
  ; MalformedQueryString
  ; InvalidQueryParameter
  ; InvalidParameterValue
  ; InvalidParameterCombination
  ; InvalidClientTokenId
  ; InvalidAction
  ; InternalFailure
  ; IncompleteSignature
  ]

let to_http_code e =
  match e with
  | AuthFailure -> None
  | AuthorizationAlreadyExists -> Some 400
  | AuthorizationNotFound -> Some 404
  | AuthorizationQuotaExceeded -> Some 400
  | BackupPolicyNotFoundFault -> Some 404
  | Blocked -> None
  | BlueGreenDeploymentAlreadyExistsFault -> Some 400
  | BlueGreenDeploymentNotFoundFault -> Some 404
  | CertificateNotFound -> Some 404
  | CreateCustomDBEngineVersionFault -> Some 400
  | CustomAvailabilityZoneNotFound -> Some 404
  | CustomDBEngineVersionAlreadyExistsFault -> Some 400
  | CustomDBEngineVersionNotFoundFault -> Some 404
  | CustomDBEngineVersionQuotaExceededFault -> Some 400
  | DBClusterAlreadyExistsFault -> Some 400
  | DBClusterAutomatedBackupNotFoundFault -> Some 404
  | DBClusterAutomatedBackupQuotaExceededFault -> Some 400
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
  | DBInstanceNotReady -> Some 400
  | DBInstanceRoleAlreadyExists -> Some 400
  | DBInstanceRoleNotFound -> Some 404
  | DBInstanceRoleQuotaExceeded -> Some 400
  | DBLogFileNotFoundFault -> Some 404
  | DBParameterGroupAlreadyExists -> Some 400
  | DBParameterGroupNotFound -> Some 404
  | DBParameterGroupQuotaExceeded -> Some 400
  | DBProxyAlreadyExistsFault -> Some 400
  | DBProxyEndpointAlreadyExistsFault -> Some 400
  | DBProxyEndpointNotFoundFault -> Some 404
  | DBProxyEndpointQuotaExceededFault -> Some 400
  | DBProxyNotFoundFault -> Some 404
  | DBProxyQuotaExceededFault -> Some 400
  | DBProxyTargetAlreadyRegisteredFault -> Some 400
  | DBProxyTargetGroupNotFoundFault -> Some 404
  | DBProxyTargetNotFoundFault -> Some 404
  | DBSecurityGroupAlreadyExists -> Some 400
  | DBSecurityGroupNotFound -> Some 404
  | DBSecurityGroupNotSupported -> Some 400
  | DBShardGroupAlreadyExists -> Some 400
  | DBShardGroupNotFound -> Some 404
  | DBSnapshotAlreadyExists -> Some 400
  | DBSnapshotNotFound -> Some 404
  | DBSnapshotTenantDatabaseNotFoundFault -> Some 404
  | DBSubnetGroupAlreadyExists -> Some 400
  | DBSubnetGroupDoesNotCoverEnoughAZs -> Some 400
  | DBSubnetGroupNotAllowedFault -> Some 400
  | DBSubnetGroupNotFoundFault -> Some 404
  | DBSubnetGroupQuotaExceeded -> Some 400
  | DBSubnetQuotaExceededFault -> Some 400
  | DBUpgradeDependencyFailure -> Some 400
  | DomainNotFoundFault -> Some 404
  | DryRunOperation -> None
  | Ec2ImagePropertiesNotSupportedFault -> Some 400
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
  | InstanceQuotaExceeded -> Some 400
  | InsufficientAvailableIPsInSubnetFault -> Some 400
  | InsufficientDBClusterCapacityFault -> Some 403
  | InsufficientDBInstanceCapacity -> Some 400
  | InsufficientStorageClusterCapacity -> Some 400
  | IntegrationAlreadyExistsFault -> Some 400
  | IntegrationConflictOperationFault -> Some 400
  | IntegrationNotFoundFault -> Some 404
  | IntegrationQuotaExceededFault -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidBlueGreenDeploymentStateFault -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidCustomDBEngineVersionStateFault -> Some 400
  | InvalidDBClusterAutomatedBackupStateFault -> Some 400
  | InvalidDBClusterCapacityFault -> Some 400
  | InvalidDBClusterEndpointStateFault -> Some 400
  | InvalidDBClusterSnapshotStateFault -> Some 400
  | InvalidDBClusterStateFault -> Some 400
  | InvalidDBInstanceAutomatedBackupState -> Some 400
  | InvalidDBInstanceState -> Some 400
  | InvalidDBParameterGroupState -> Some 400
  | InvalidDBProxyEndpointStateFault -> Some 400
  | InvalidDBProxyStateFault -> Some 400
  | InvalidDBSecurityGroupState -> Some 400
  | InvalidDBShardGroupState -> Some 400
  | InvalidDBSnapshotState -> Some 400
  | InvalidDBSubnetGroupFault -> Some 400
  | InvalidDBSubnetGroupStateFault -> Some 400
  | InvalidDBSubnetStateFault -> Some 400
  | InvalidEventSubscriptionState -> Some 400
  | InvalidExportOnly -> Some 400
  | InvalidExportSourceState -> Some 400
  | InvalidExportTaskStateFault -> Some 400
  | InvalidGlobalClusterStateFault -> Some 400
  | InvalidIntegrationStateFault -> Some 400
  | InvalidOptionGroupStateFault -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | InvalidResourceStateFault -> Some 400
  | InvalidRestoreFault -> Some 400
  | InvalidS3BucketFault -> Some 400
  | InvalidSubnet -> Some 400
  | InvalidVPCNetworkStateFault -> Some 400
  | KMSKeyNotAccessibleFault -> Some 400
  | MalformedQueryString -> Some 404
  | MaxDBShardGroupLimitReached -> Some 400
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NetworkTypeNotSupported -> Some 400
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
  | SourceClusterNotSupportedFault -> Some 400
  | SourceDatabaseNotSupportedFault -> Some 400
  | SourceNotFound -> Some 404
  | StorageQuotaExceeded -> Some 400
  | StorageTypeNotAvailableFault -> Some 400
  | StorageTypeNotSupported -> Some 400
  | SubnetAlreadyInUse -> Some 400
  | SubscriptionAlreadyExist -> Some 400
  | SubscriptionCategoryNotFound -> Some 404
  | SubscriptionNotFound -> Some 404
  | TenantDatabaseAlreadyExists -> Some 400
  | TenantDatabaseNotFound -> Some 404
  | TenantDatabaseQuotaExceeded -> Some 400
  | Throttling -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedDBEngineVersion -> Some 400
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | VpcEncryptionControlViolationException -> Some 400
  | Uninhabited -> None

let to_string e =
  match e with
  | AuthFailure -> "AuthFailure"
  | AuthorizationAlreadyExists -> "AuthorizationAlreadyExists"
  | AuthorizationNotFound -> "AuthorizationNotFound"
  | AuthorizationQuotaExceeded -> "AuthorizationQuotaExceeded"
  | BackupPolicyNotFoundFault -> "BackupPolicyNotFoundFault"
  | Blocked -> "Blocked"
  | BlueGreenDeploymentAlreadyExistsFault -> "BlueGreenDeploymentAlreadyExistsFault"
  | BlueGreenDeploymentNotFoundFault -> "BlueGreenDeploymentNotFoundFault"
  | CertificateNotFound -> "CertificateNotFound"
  | CreateCustomDBEngineVersionFault -> "CreateCustomDBEngineVersionFault"
  | CustomAvailabilityZoneNotFound -> "CustomAvailabilityZoneNotFound"
  | CustomDBEngineVersionAlreadyExistsFault -> "CustomDBEngineVersionAlreadyExistsFault"
  | CustomDBEngineVersionNotFoundFault -> "CustomDBEngineVersionNotFoundFault"
  | CustomDBEngineVersionQuotaExceededFault -> "CustomDBEngineVersionQuotaExceededFault"
  | DBClusterAlreadyExistsFault -> "DBClusterAlreadyExistsFault"
  | DBClusterAutomatedBackupNotFoundFault -> "DBClusterAutomatedBackupNotFoundFault"
  | DBClusterAutomatedBackupQuotaExceededFault ->
      "DBClusterAutomatedBackupQuotaExceededFault"
  | DBClusterBacktrackNotFoundFault -> "DBClusterBacktrackNotFoundFault"
  | DBClusterEndpointAlreadyExistsFault -> "DBClusterEndpointAlreadyExistsFault"
  | DBClusterEndpointNotFoundFault -> "DBClusterEndpointNotFoundFault"
  | DBClusterEndpointQuotaExceededFault -> "DBClusterEndpointQuotaExceededFault"
  | DBClusterNotFoundFault -> "DBClusterNotFoundFault"
  | DBClusterParameterGroupNotFound -> "DBClusterParameterGroupNotFound"
  | DBClusterQuotaExceededFault -> "DBClusterQuotaExceededFault"
  | DBClusterRoleAlreadyExists -> "DBClusterRoleAlreadyExists"
  | DBClusterRoleNotFound -> "DBClusterRoleNotFound"
  | DBClusterRoleQuotaExceeded -> "DBClusterRoleQuotaExceeded"
  | DBClusterSnapshotAlreadyExistsFault -> "DBClusterSnapshotAlreadyExistsFault"
  | DBClusterSnapshotNotFoundFault -> "DBClusterSnapshotNotFoundFault"
  | DBInstanceAlreadyExists -> "DBInstanceAlreadyExists"
  | DBInstanceAutomatedBackupNotFound -> "DBInstanceAutomatedBackupNotFound"
  | DBInstanceAutomatedBackupQuotaExceeded -> "DBInstanceAutomatedBackupQuotaExceeded"
  | DBInstanceNotFound -> "DBInstanceNotFound"
  | DBInstanceNotReady -> "DBInstanceNotReady"
  | DBInstanceRoleAlreadyExists -> "DBInstanceRoleAlreadyExists"
  | DBInstanceRoleNotFound -> "DBInstanceRoleNotFound"
  | DBInstanceRoleQuotaExceeded -> "DBInstanceRoleQuotaExceeded"
  | DBLogFileNotFoundFault -> "DBLogFileNotFoundFault"
  | DBParameterGroupAlreadyExists -> "DBParameterGroupAlreadyExists"
  | DBParameterGroupNotFound -> "DBParameterGroupNotFound"
  | DBParameterGroupQuotaExceeded -> "DBParameterGroupQuotaExceeded"
  | DBProxyAlreadyExistsFault -> "DBProxyAlreadyExistsFault"
  | DBProxyEndpointAlreadyExistsFault -> "DBProxyEndpointAlreadyExistsFault"
  | DBProxyEndpointNotFoundFault -> "DBProxyEndpointNotFoundFault"
  | DBProxyEndpointQuotaExceededFault -> "DBProxyEndpointQuotaExceededFault"
  | DBProxyNotFoundFault -> "DBProxyNotFoundFault"
  | DBProxyQuotaExceededFault -> "DBProxyQuotaExceededFault"
  | DBProxyTargetAlreadyRegisteredFault -> "DBProxyTargetAlreadyRegisteredFault"
  | DBProxyTargetGroupNotFoundFault -> "DBProxyTargetGroupNotFoundFault"
  | DBProxyTargetNotFoundFault -> "DBProxyTargetNotFoundFault"
  | DBSecurityGroupAlreadyExists -> "DBSecurityGroupAlreadyExists"
  | DBSecurityGroupNotFound -> "DBSecurityGroupNotFound"
  | DBSecurityGroupNotSupported -> "DBSecurityGroupNotSupported"
  | DBShardGroupAlreadyExists -> "DBShardGroupAlreadyExists"
  | DBShardGroupNotFound -> "DBShardGroupNotFound"
  | DBSnapshotAlreadyExists -> "DBSnapshotAlreadyExists"
  | DBSnapshotNotFound -> "DBSnapshotNotFound"
  | DBSnapshotTenantDatabaseNotFoundFault -> "DBSnapshotTenantDatabaseNotFoundFault"
  | DBSubnetGroupAlreadyExists -> "DBSubnetGroupAlreadyExists"
  | DBSubnetGroupDoesNotCoverEnoughAZs -> "DBSubnetGroupDoesNotCoverEnoughAZs"
  | DBSubnetGroupNotAllowedFault -> "DBSubnetGroupNotAllowedFault"
  | DBSubnetGroupNotFoundFault -> "DBSubnetGroupNotFoundFault"
  | DBSubnetGroupQuotaExceeded -> "DBSubnetGroupQuotaExceeded"
  | DBSubnetQuotaExceededFault -> "DBSubnetQuotaExceededFault"
  | DBUpgradeDependencyFailure -> "DBUpgradeDependencyFailure"
  | DomainNotFoundFault -> "DomainNotFoundFault"
  | DryRunOperation -> "DryRunOperation"
  | Ec2ImagePropertiesNotSupportedFault -> "Ec2ImagePropertiesNotSupportedFault"
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
  | InstanceQuotaExceeded -> "InstanceQuotaExceeded"
  | InsufficientAvailableIPsInSubnetFault -> "InsufficientAvailableIPsInSubnetFault"
  | InsufficientDBClusterCapacityFault -> "InsufficientDBClusterCapacityFault"
  | InsufficientDBInstanceCapacity -> "InsufficientDBInstanceCapacity"
  | InsufficientStorageClusterCapacity -> "InsufficientStorageClusterCapacity"
  | IntegrationAlreadyExistsFault -> "IntegrationAlreadyExistsFault"
  | IntegrationConflictOperationFault -> "IntegrationConflictOperationFault"
  | IntegrationNotFoundFault -> "IntegrationNotFoundFault"
  | IntegrationQuotaExceededFault -> "IntegrationQuotaExceededFault"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidBlueGreenDeploymentStateFault -> "InvalidBlueGreenDeploymentStateFault"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidCustomDBEngineVersionStateFault -> "InvalidCustomDBEngineVersionStateFault"
  | InvalidDBClusterAutomatedBackupStateFault ->
      "InvalidDBClusterAutomatedBackupStateFault"
  | InvalidDBClusterCapacityFault -> "InvalidDBClusterCapacityFault"
  | InvalidDBClusterEndpointStateFault -> "InvalidDBClusterEndpointStateFault"
  | InvalidDBClusterSnapshotStateFault -> "InvalidDBClusterSnapshotStateFault"
  | InvalidDBClusterStateFault -> "InvalidDBClusterStateFault"
  | InvalidDBInstanceAutomatedBackupState -> "InvalidDBInstanceAutomatedBackupState"
  | InvalidDBInstanceState -> "InvalidDBInstanceState"
  | InvalidDBParameterGroupState -> "InvalidDBParameterGroupState"
  | InvalidDBProxyEndpointStateFault -> "InvalidDBProxyEndpointStateFault"
  | InvalidDBProxyStateFault -> "InvalidDBProxyStateFault"
  | InvalidDBSecurityGroupState -> "InvalidDBSecurityGroupState"
  | InvalidDBShardGroupState -> "InvalidDBShardGroupState"
  | InvalidDBSnapshotState -> "InvalidDBSnapshotState"
  | InvalidDBSubnetGroupFault -> "InvalidDBSubnetGroupFault"
  | InvalidDBSubnetGroupStateFault -> "InvalidDBSubnetGroupStateFault"
  | InvalidDBSubnetStateFault -> "InvalidDBSubnetStateFault"
  | InvalidEventSubscriptionState -> "InvalidEventSubscriptionState"
  | InvalidExportOnly -> "InvalidExportOnly"
  | InvalidExportSourceState -> "InvalidExportSourceState"
  | InvalidExportTaskStateFault -> "InvalidExportTaskStateFault"
  | InvalidGlobalClusterStateFault -> "InvalidGlobalClusterStateFault"
  | InvalidIntegrationStateFault -> "InvalidIntegrationStateFault"
  | InvalidOptionGroupStateFault -> "InvalidOptionGroupStateFault"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidResourceStateFault -> "InvalidResourceStateFault"
  | InvalidRestoreFault -> "InvalidRestoreFault"
  | InvalidS3BucketFault -> "InvalidS3BucketFault"
  | InvalidSubnet -> "InvalidSubnet"
  | InvalidVPCNetworkStateFault -> "InvalidVPCNetworkStateFault"
  | KMSKeyNotAccessibleFault -> "KMSKeyNotAccessibleFault"
  | MalformedQueryString -> "MalformedQueryString"
  | MaxDBShardGroupLimitReached -> "MaxDBShardGroupLimitReached"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NetworkTypeNotSupported -> "NetworkTypeNotSupported"
  | OptInRequired -> "OptInRequired"
  | OptionGroupAlreadyExistsFault -> "OptionGroupAlreadyExistsFault"
  | OptionGroupNotFoundFault -> "OptionGroupNotFoundFault"
  | OptionGroupQuotaExceededFault -> "OptionGroupQuotaExceededFault"
  | PendingVerification -> "PendingVerification"
  | PointInTimeRestoreNotEnabled -> "PointInTimeRestoreNotEnabled"
  | ProvisionedIopsNotAvailableInAZFault -> "ProvisionedIopsNotAvailableInAZFault"
  | QuotaExceeded_DBSecurityGroup -> "QuotaExceeded.DBSecurityGroup"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ReservedDBInstanceAlreadyExists -> "ReservedDBInstanceAlreadyExists"
  | ReservedDBInstanceNotFound -> "ReservedDBInstanceNotFound"
  | ReservedDBInstanceQuotaExceeded -> "ReservedDBInstanceQuotaExceeded"
  | ReservedDBInstancesOfferingNotFound -> "ReservedDBInstancesOfferingNotFound"
  | ResourceNotFoundFault -> "ResourceNotFoundFault"
  | SNSInvalidTopic -> "SNSInvalidTopic"
  | SNSNoAuthorization -> "SNSNoAuthorization"
  | SNSTopicArnNotFound -> "SNSTopicArnNotFound"
  | ServiceUnavailable -> "ServiceUnavailable"
  | SharedSnapshotQuotaExceeded -> "SharedSnapshotQuotaExceeded"
  | SnapshotQuotaExceeded -> "SnapshotQuotaExceeded"
  | SourceClusterNotSupportedFault -> "SourceClusterNotSupportedFault"
  | SourceDatabaseNotSupportedFault -> "SourceDatabaseNotSupportedFault"
  | SourceNotFound -> "SourceNotFound"
  | StorageQuotaExceeded -> "StorageQuotaExceeded"
  | StorageTypeNotAvailableFault -> "StorageTypeNotAvailableFault"
  | StorageTypeNotSupported -> "StorageTypeNotSupported"
  | SubnetAlreadyInUse -> "SubnetAlreadyInUse"
  | SubscriptionAlreadyExist -> "SubscriptionAlreadyExist"
  | SubscriptionCategoryNotFound -> "SubscriptionCategoryNotFound"
  | SubscriptionNotFound -> "SubscriptionNotFound"
  | TenantDatabaseAlreadyExists -> "TenantDatabaseAlreadyExists"
  | TenantDatabaseNotFound -> "TenantDatabaseNotFound"
  | TenantDatabaseQuotaExceeded -> "TenantDatabaseQuotaExceeded"
  | Throttling -> "Throttling"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedDBEngineVersion -> "UnsupportedDBEngineVersion"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | VpcEncryptionControlViolationException -> "VpcEncryptionControlViolationException"
  | Uninhabited -> "Uninhabited"

let of_string e =
  match e with
  | "AuthFailure" -> Some AuthFailure
  | "AuthorizationAlreadyExists" -> Some AuthorizationAlreadyExists
  | "AuthorizationNotFound" -> Some AuthorizationNotFound
  | "AuthorizationQuotaExceeded" -> Some AuthorizationQuotaExceeded
  | "BackupPolicyNotFoundFault" -> Some BackupPolicyNotFoundFault
  | "Blocked" -> Some Blocked
  | "BlueGreenDeploymentAlreadyExistsFault" -> Some BlueGreenDeploymentAlreadyExistsFault
  | "BlueGreenDeploymentNotFoundFault" -> Some BlueGreenDeploymentNotFoundFault
  | "CertificateNotFound" -> Some CertificateNotFound
  | "CreateCustomDBEngineVersionFault" -> Some CreateCustomDBEngineVersionFault
  | "CustomAvailabilityZoneNotFound" -> Some CustomAvailabilityZoneNotFound
  | "CustomDBEngineVersionAlreadyExistsFault" ->
      Some CustomDBEngineVersionAlreadyExistsFault
  | "CustomDBEngineVersionNotFoundFault" -> Some CustomDBEngineVersionNotFoundFault
  | "CustomDBEngineVersionQuotaExceededFault" ->
      Some CustomDBEngineVersionQuotaExceededFault
  | "DBClusterAlreadyExistsFault" -> Some DBClusterAlreadyExistsFault
  | "DBClusterAutomatedBackupNotFoundFault" -> Some DBClusterAutomatedBackupNotFoundFault
  | "DBClusterAutomatedBackupQuotaExceededFault" ->
      Some DBClusterAutomatedBackupQuotaExceededFault
  | "DBClusterBacktrackNotFoundFault" -> Some DBClusterBacktrackNotFoundFault
  | "DBClusterEndpointAlreadyExistsFault" -> Some DBClusterEndpointAlreadyExistsFault
  | "DBClusterEndpointNotFoundFault" -> Some DBClusterEndpointNotFoundFault
  | "DBClusterEndpointQuotaExceededFault" -> Some DBClusterEndpointQuotaExceededFault
  | "DBClusterNotFoundFault" -> Some DBClusterNotFoundFault
  | "DBClusterParameterGroupNotFound" -> Some DBClusterParameterGroupNotFound
  | "DBClusterQuotaExceededFault" -> Some DBClusterQuotaExceededFault
  | "DBClusterRoleAlreadyExists" -> Some DBClusterRoleAlreadyExists
  | "DBClusterRoleNotFound" -> Some DBClusterRoleNotFound
  | "DBClusterRoleQuotaExceeded" -> Some DBClusterRoleQuotaExceeded
  | "DBClusterSnapshotAlreadyExistsFault" -> Some DBClusterSnapshotAlreadyExistsFault
  | "DBClusterSnapshotNotFoundFault" -> Some DBClusterSnapshotNotFoundFault
  | "DBInstanceAlreadyExists" -> Some DBInstanceAlreadyExists
  | "DBInstanceAutomatedBackupNotFound" -> Some DBInstanceAutomatedBackupNotFound
  | "DBInstanceAutomatedBackupQuotaExceeded" ->
      Some DBInstanceAutomatedBackupQuotaExceeded
  | "DBInstanceNotFound" -> Some DBInstanceNotFound
  | "DBInstanceNotReady" -> Some DBInstanceNotReady
  | "DBInstanceRoleAlreadyExists" -> Some DBInstanceRoleAlreadyExists
  | "DBInstanceRoleNotFound" -> Some DBInstanceRoleNotFound
  | "DBInstanceRoleQuotaExceeded" -> Some DBInstanceRoleQuotaExceeded
  | "DBLogFileNotFoundFault" -> Some DBLogFileNotFoundFault
  | "DBParameterGroupAlreadyExists" -> Some DBParameterGroupAlreadyExists
  | "DBParameterGroupNotFound" -> Some DBParameterGroupNotFound
  | "DBParameterGroupQuotaExceeded" -> Some DBParameterGroupQuotaExceeded
  | "DBProxyAlreadyExistsFault" -> Some DBProxyAlreadyExistsFault
  | "DBProxyEndpointAlreadyExistsFault" -> Some DBProxyEndpointAlreadyExistsFault
  | "DBProxyEndpointNotFoundFault" -> Some DBProxyEndpointNotFoundFault
  | "DBProxyEndpointQuotaExceededFault" -> Some DBProxyEndpointQuotaExceededFault
  | "DBProxyNotFoundFault" -> Some DBProxyNotFoundFault
  | "DBProxyQuotaExceededFault" -> Some DBProxyQuotaExceededFault
  | "DBProxyTargetAlreadyRegisteredFault" -> Some DBProxyTargetAlreadyRegisteredFault
  | "DBProxyTargetGroupNotFoundFault" -> Some DBProxyTargetGroupNotFoundFault
  | "DBProxyTargetNotFoundFault" -> Some DBProxyTargetNotFoundFault
  | "DBSecurityGroupAlreadyExists" -> Some DBSecurityGroupAlreadyExists
  | "DBSecurityGroupNotFound" -> Some DBSecurityGroupNotFound
  | "DBSecurityGroupNotSupported" -> Some DBSecurityGroupNotSupported
  | "DBShardGroupAlreadyExists" -> Some DBShardGroupAlreadyExists
  | "DBShardGroupNotFound" -> Some DBShardGroupNotFound
  | "DBSnapshotAlreadyExists" -> Some DBSnapshotAlreadyExists
  | "DBSnapshotNotFound" -> Some DBSnapshotNotFound
  | "DBSnapshotTenantDatabaseNotFoundFault" -> Some DBSnapshotTenantDatabaseNotFoundFault
  | "DBSubnetGroupAlreadyExists" -> Some DBSubnetGroupAlreadyExists
  | "DBSubnetGroupDoesNotCoverEnoughAZs" -> Some DBSubnetGroupDoesNotCoverEnoughAZs
  | "DBSubnetGroupNotAllowedFault" -> Some DBSubnetGroupNotAllowedFault
  | "DBSubnetGroupNotFoundFault" -> Some DBSubnetGroupNotFoundFault
  | "DBSubnetGroupQuotaExceeded" -> Some DBSubnetGroupQuotaExceeded
  | "DBSubnetQuotaExceededFault" -> Some DBSubnetQuotaExceededFault
  | "DBUpgradeDependencyFailure" -> Some DBUpgradeDependencyFailure
  | "DomainNotFoundFault" -> Some DomainNotFoundFault
  | "DryRunOperation" -> Some DryRunOperation
  | "Ec2ImagePropertiesNotSupportedFault" -> Some Ec2ImagePropertiesNotSupportedFault
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
  | "InstanceQuotaExceeded" -> Some InstanceQuotaExceeded
  | "InsufficientAvailableIPsInSubnetFault" -> Some InsufficientAvailableIPsInSubnetFault
  | "InsufficientDBClusterCapacityFault" -> Some InsufficientDBClusterCapacityFault
  | "InsufficientDBInstanceCapacity" -> Some InsufficientDBInstanceCapacity
  | "InsufficientStorageClusterCapacity" -> Some InsufficientStorageClusterCapacity
  | "IntegrationAlreadyExistsFault" -> Some IntegrationAlreadyExistsFault
  | "IntegrationConflictOperationFault" -> Some IntegrationConflictOperationFault
  | "IntegrationNotFoundFault" -> Some IntegrationNotFoundFault
  | "IntegrationQuotaExceededFault" -> Some IntegrationQuotaExceededFault
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidBlueGreenDeploymentStateFault" -> Some InvalidBlueGreenDeploymentStateFault
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidCustomDBEngineVersionStateFault" ->
      Some InvalidCustomDBEngineVersionStateFault
  | "InvalidDBClusterAutomatedBackupStateFault" ->
      Some InvalidDBClusterAutomatedBackupStateFault
  | "InvalidDBClusterCapacityFault" -> Some InvalidDBClusterCapacityFault
  | "InvalidDBClusterEndpointStateFault" -> Some InvalidDBClusterEndpointStateFault
  | "InvalidDBClusterSnapshotStateFault" -> Some InvalidDBClusterSnapshotStateFault
  | "InvalidDBClusterStateFault" -> Some InvalidDBClusterStateFault
  | "InvalidDBInstanceAutomatedBackupState" -> Some InvalidDBInstanceAutomatedBackupState
  | "InvalidDBInstanceState" -> Some InvalidDBInstanceState
  | "InvalidDBParameterGroupState" -> Some InvalidDBParameterGroupState
  | "InvalidDBProxyEndpointStateFault" -> Some InvalidDBProxyEndpointStateFault
  | "InvalidDBProxyStateFault" -> Some InvalidDBProxyStateFault
  | "InvalidDBSecurityGroupState" -> Some InvalidDBSecurityGroupState
  | "InvalidDBShardGroupState" -> Some InvalidDBShardGroupState
  | "InvalidDBSnapshotState" -> Some InvalidDBSnapshotState
  | "InvalidDBSubnetGroupFault" -> Some InvalidDBSubnetGroupFault
  | "InvalidDBSubnetGroupStateFault" -> Some InvalidDBSubnetGroupStateFault
  | "InvalidDBSubnetStateFault" -> Some InvalidDBSubnetStateFault
  | "InvalidEventSubscriptionState" -> Some InvalidEventSubscriptionState
  | "InvalidExportOnly" -> Some InvalidExportOnly
  | "InvalidExportSourceState" -> Some InvalidExportSourceState
  | "InvalidExportTaskStateFault" -> Some InvalidExportTaskStateFault
  | "InvalidGlobalClusterStateFault" -> Some InvalidGlobalClusterStateFault
  | "InvalidIntegrationStateFault" -> Some InvalidIntegrationStateFault
  | "InvalidOptionGroupStateFault" -> Some InvalidOptionGroupStateFault
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidResourceStateFault" -> Some InvalidResourceStateFault
  | "InvalidRestoreFault" -> Some InvalidRestoreFault
  | "InvalidS3BucketFault" -> Some InvalidS3BucketFault
  | "InvalidSubnet" -> Some InvalidSubnet
  | "InvalidVPCNetworkStateFault" -> Some InvalidVPCNetworkStateFault
  | "KMSKeyNotAccessibleFault" -> Some KMSKeyNotAccessibleFault
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MaxDBShardGroupLimitReached" -> Some MaxDBShardGroupLimitReached
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NetworkTypeNotSupported" -> Some NetworkTypeNotSupported
  | "OptInRequired" -> Some OptInRequired
  | "OptionGroupAlreadyExistsFault" -> Some OptionGroupAlreadyExistsFault
  | "OptionGroupNotFoundFault" -> Some OptionGroupNotFoundFault
  | "OptionGroupQuotaExceededFault" -> Some OptionGroupQuotaExceededFault
  | "PendingVerification" -> Some PendingVerification
  | "PointInTimeRestoreNotEnabled" -> Some PointInTimeRestoreNotEnabled
  | "ProvisionedIopsNotAvailableInAZFault" -> Some ProvisionedIopsNotAvailableInAZFault
  | "QuotaExceeded.DBSecurityGroup" -> Some QuotaExceeded_DBSecurityGroup
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ReservedDBInstanceAlreadyExists" -> Some ReservedDBInstanceAlreadyExists
  | "ReservedDBInstanceNotFound" -> Some ReservedDBInstanceNotFound
  | "ReservedDBInstanceQuotaExceeded" -> Some ReservedDBInstanceQuotaExceeded
  | "ReservedDBInstancesOfferingNotFound" -> Some ReservedDBInstancesOfferingNotFound
  | "ResourceNotFoundFault" -> Some ResourceNotFoundFault
  | "SNSInvalidTopic" -> Some SNSInvalidTopic
  | "SNSNoAuthorization" -> Some SNSNoAuthorization
  | "SNSTopicArnNotFound" -> Some SNSTopicArnNotFound
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "SharedSnapshotQuotaExceeded" -> Some SharedSnapshotQuotaExceeded
  | "SnapshotQuotaExceeded" -> Some SnapshotQuotaExceeded
  | "SourceClusterNotSupportedFault" -> Some SourceClusterNotSupportedFault
  | "SourceDatabaseNotSupportedFault" -> Some SourceDatabaseNotSupportedFault
  | "SourceNotFound" -> Some SourceNotFound
  | "StorageQuotaExceeded" -> Some StorageQuotaExceeded
  | "StorageTypeNotAvailableFault" -> Some StorageTypeNotAvailableFault
  | "StorageTypeNotSupported" -> Some StorageTypeNotSupported
  | "SubnetAlreadyInUse" -> Some SubnetAlreadyInUse
  | "SubscriptionAlreadyExist" -> Some SubscriptionAlreadyExist
  | "SubscriptionCategoryNotFound" -> Some SubscriptionCategoryNotFound
  | "SubscriptionNotFound" -> Some SubscriptionNotFound
  | "TenantDatabaseAlreadyExists" -> Some TenantDatabaseAlreadyExists
  | "TenantDatabaseNotFound" -> Some TenantDatabaseNotFound
  | "TenantDatabaseQuotaExceeded" -> Some TenantDatabaseQuotaExceeded
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedDBEngineVersion" -> Some UnsupportedDBEngineVersion
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "VpcEncryptionControlViolationException" ->
      Some VpcEncryptionControlViolationException
  | "Uninhabited" -> Some Uninhabited
  | _ -> None
