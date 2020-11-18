type t =
  | ActiveVpcPeeringConnectionPerVpcLimitExceeded 
  | AddressLimitExceeded 
  | AttachmentLimitExceeded 
  | AuthFailure 
  | Blocked 
  | BundlingInProgress 
  | CannotDelete 
  | ConcurrentSnapshotLimitExceeded 
  | ConcurrentTagAccess 
  | CustomerGatewayLimitExceeded 
  | DependencyViolation 
  | DiskImageSizeTooLarge 
  | DryRunOperation 
  | EncryptedVolumesNotSupported 
  | FilterLimitExceeded 
  | Gateway_NotAttached 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | IncorrectInstanceState 
  | IncorrectState 
  | InstanceAlreadyLinked 
  | InstanceLimitExceeded 
  | InsufficientFreeAddressesInSubnet 
  | InsufficientReservedInstancesCapacity 
  | InternalFailure 
  | InternetGatewayLimitExceeded 
  | InvalidAMIAttributeItemValue 
  | InvalidAMIID_Malformed 
  | InvalidAMIID_NotFound 
  | InvalidAMIID_Unavailable 
  | InvalidAMIName_Duplicate 
  | InvalidAMIName_Malformed 
  | InvalidAction 
  | InvalidAddress_NotFound 
  | InvalidAddressID_NotFound 
  | InvalidAllocationID_NotFound 
  | InvalidAssociationID_NotFound 
  | InvalidAttachment_NotFound 
  | InvalidAttachmentID_NotFound 
  | InvalidBlockDeviceMapping 
  | InvalidBundleID_NotFound 
  | InvalidClientTokenId 
  | InvalidConversionTaskId 
  | InvalidCustomerGateway_DuplicateIpAddress 
  | InvalidCustomerGatewayID_NotFound 
  | InvalidCustomerGatewayId_Malformed 
  | InvalidDevice_InUse 
  | InvalidDhcpOptionID_NotFound 
  | InvalidDhcpOptionsID_NotFound 
  | InvalidDhcpOptionsId_Malformed 
  | InvalidExportTaskID_NotFound 
  | InvalidFilter 
  | InvalidFormat 
  | InvalidGatewayID_NotFound 
  | InvalidGroup_Duplicate 
  | InvalidGroup_InUse 
  | InvalidGroup_NotFound 
  | InvalidGroup_Reserved 
  | InvalidGroupId_Malformed 
  | InvalidID 
  | InvalidIPAddress_InUse 
  | InvalidInput 
  | InvalidInstanceAttributeValue 
  | InvalidInstanceID 
  | InvalidInstanceID_Malformed 
  | InvalidInstanceID_NotFound 
  | InvalidInstanceID_NotLinkable 
  | InvalidInstanceType 
  | InvalidInterface_IpAddressLimitExceeded 
  | InvalidInternetGatewayID_NotFound 
  | InvalidKey_Format 
  | InvalidKeyPair_Duplicate 
  | InvalidKeyPair_Format 
  | InvalidKeyPair_NotFound 
  | InvalidManifest 
  | InvalidNetworkAclEntry_NotFound 
  | InvalidNetworkAclID_NotFound 
  | InvalidNetworkInterface_InUse 
  | InvalidNetworkInterfaceAttachmentID_Malformed 
  | InvalidNetworkInterfaceID_NotFound 
  | InvalidNetworkInterfaceId_Malformed 
  | InvalidOption_Conflict 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidPermission_Duplicate 
  | InvalidPermission_Malformed 
  | InvalidPermission_NotFound 
  | InvalidPlacementGroup_Duplicate 
  | InvalidPlacementGroup_InUse 
  | InvalidPlacementGroup_Unknown 
  | InvalidQueryParameter 
  | InvalidRequest 
  | InvalidReservationID_Malformed 
  | InvalidReservationID_NotFound 
  | InvalidReservedInstancesId 
  | InvalidReservedInstancesOfferingId 
  | InvalidRoute_Malformed 
  | InvalidRoute_NotFound 
  | InvalidRouteTableID_NotFound 
  | InvalidRouteTableId_Malformed 
  | InvalidSecurity_RequestHasExpired 
  | InvalidSecurityGroupID_NotFound 
  | InvalidSnapshot_InUse 
  | InvalidSnapshot_NotFound 
  | InvalidSnapshotID_Malformed 
  | InvalidSpotDatafeed_NotFound 
  | InvalidSpotInstanceRequestID_Malformed 
  | InvalidSpotInstanceRequestID_NotFound 
  | InvalidState 
  | InvalidStateTransition 
  | InvalidSubnet_Conflict 
  | InvalidSubnetID_NotFound 
  | InvalidUserID_Malformed 
  | InvalidVolume_NotFound 
  | InvalidVolume_ZoneMismatch 
  | InvalidVolumeID_Duplicate 
  | InvalidVolumeID_Malformed 
  | InvalidVolumeID_ZoneMismatch 
  | InvalidVpcID_NotFound 
  | InvalidVpcPeeringConnectionID_NotFound 
  | InvalidVpcPeeringConnectionId_Malformed 
  | InvalidVpcRange 
  | InvalidVpcState 
  | InvalidVpnConnectionID 
  | InvalidVpnConnectionID_NotFound 
  | InvalidVpnGatewayAttachment_NotFound 
  | InvalidVpnGatewayID_NotFound 
  | InvalidZone_NotFound 
  | LegacySecurityGroup 
  | MalformedQueryString 
  | MaxIOPSLimitExceeded 
  | MaxSpotInstanceCountExceeded 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | NetworkAclEntryAlreadyExists 
  | NetworkAclEntryLimitExceeded 
  | NetworkAclLimitExceeded 
  | NonEBSInstance 
  | NotExportable 
  | OperationNotPermitted 
  | OptInRequired 
  | OutstandingVpcPeeringConnectionLimitExceeded 
  | PendingSnapshotLimitExceeded 
  | PendingVerification 
  | PrivateIpAddressLimitExceeded 
  | RequestExpired 
  | RequestLimitExceeded 
  | RequestResourceCountExceeded 
  | ReservedInstancesLimitExceeded 
  | Resource_AlreadyAssociated 
  | ResourceCountExceeded 
  | ResourceLimitExceeded 
  | RouteAlreadyExists 
  | RouteLimitExceeded 
  | RouteTableLimitExceeded 
  | RulesPerSecurityGroupLimitExceeded 
  | SecurityGroupLimitExceeded 
  | SecurityGroupsPerInstanceLimitExceeded 
  | SecurityGroupsPerInterfaceLimitExceeded 
  | ServiceUnavailable 
  | SignatureDoesNotMatch 
  | SnapshotLimitExceeded 
  | SubnetLimitExceeded 
  | TagLimitExceeded 
  | Throttling 
  | UnauthorizedOperation 
  | UnknownParameter 
  | UnknownVolumeType 
  | Unsupported 
  | UnsupportedOperation 
  | UnsupportedProtocol 
  | VPCIdNotSpecified 
  | VPCResourceNotSpecified 
  | ValidationError 
  | VolumeInUse 
  | VolumeLimitExceeded 
  | VolumeTypeNotAvailableInZone 
  | VpcCidrConflict 
  | VpcLimitExceeded 
  | VpcPeeringConnectionAlreadyExists 
  | VpnConnectionLimitExceeded 
  | VpnGatewayAttachmentLimitExceeded 
  | VpnGatewayLimitExceeded 
  | Uninhabited 
let common =
  [VpnGatewayLimitExceeded;
  VpnGatewayAttachmentLimitExceeded;
  VpnConnectionLimitExceeded;
  VPCResourceNotSpecified;
  VpcPeeringConnectionAlreadyExists;
  VpcLimitExceeded;
  VPCIdNotSpecified;
  VpcCidrConflict;
  VolumeTypeNotAvailableInZone;
  VolumeLimitExceeded;
  VolumeInUse;
  UnsupportedOperation;
  Unsupported;
  UnknownVolumeType;
  TagLimitExceeded;
  SubnetLimitExceeded;
  SnapshotLimitExceeded;
  SignatureDoesNotMatch;
  SecurityGroupsPerInterfaceLimitExceeded;
  SecurityGroupsPerInstanceLimitExceeded;
  SecurityGroupLimitExceeded;
  RulesPerSecurityGroupLimitExceeded;
  RouteTableLimitExceeded;
  RouteLimitExceeded;
  RouteAlreadyExists;
  ResourceLimitExceeded;
  ResourceCountExceeded;
  Resource_AlreadyAssociated;
  ReservedInstancesLimitExceeded;
  RequestResourceCountExceeded;
  PrivateIpAddressLimitExceeded;
  PendingSnapshotLimitExceeded;
  OutstandingVpcPeeringConnectionLimitExceeded;
  OperationNotPermitted;
  NotExportable;
  NonEBSInstance;
  NetworkAclLimitExceeded;
  NetworkAclEntryLimitExceeded;
  NetworkAclEntryAlreadyExists;
  MaxSpotInstanceCountExceeded;
  MaxIOPSLimitExceeded;
  LegacySecurityGroup;
  InvalidZone_NotFound;
  InvalidVpnGatewayID_NotFound;
  InvalidVpnGatewayAttachment_NotFound;
  InvalidVpnConnectionID_NotFound;
  InvalidVpnConnectionID;
  InvalidVpcState;
  InvalidVpcRange;
  InvalidVpcPeeringConnectionID_NotFound;
  InvalidVpcPeeringConnectionId_Malformed;
  InvalidVpcID_NotFound;
  InvalidVolume_ZoneMismatch;
  InvalidVolume_NotFound;
  InvalidVolumeID_ZoneMismatch;
  InvalidVolumeID_Malformed;
  InvalidVolumeID_Duplicate;
  InvalidUserID_Malformed;
  InvalidSubnetID_NotFound;
  InvalidSubnet_Conflict;
  InvalidStateTransition;
  InvalidState;
  InvalidSpotInstanceRequestID_NotFound;
  InvalidSpotInstanceRequestID_Malformed;
  InvalidSpotDatafeed_NotFound;
  InvalidSnapshot_NotFound;
  InvalidSnapshot_InUse;
  InvalidSnapshotID_Malformed;
  InvalidSecurity_RequestHasExpired;
  InvalidSecurityGroupID_NotFound;
  InvalidRouteTableID_NotFound;
  InvalidRouteTableId_Malformed;
  InvalidRoute_NotFound;
  InvalidRoute_Malformed;
  InvalidReservedInstancesOfferingId;
  InvalidReservedInstancesId;
  InvalidReservationID_NotFound;
  InvalidReservationID_Malformed;
  InvalidRequest;
  InvalidPlacementGroup_Unknown;
  InvalidPlacementGroup_InUse;
  InvalidPlacementGroup_Duplicate;
  InvalidPermission_NotFound;
  InvalidPermission_Malformed;
  InvalidPermission_Duplicate;
  InvalidOption_Conflict;
  InvalidNetworkInterfaceID_NotFound;
  InvalidNetworkInterfaceId_Malformed;
  InvalidNetworkInterface_InUse;
  InvalidNetworkInterfaceAttachmentID_Malformed;
  InvalidNetworkAclID_NotFound;
  InvalidNetworkAclEntry_NotFound;
  InvalidManifest;
  InvalidKeyPair_NotFound;
  InvalidKeyPair_Format;
  InvalidKeyPair_Duplicate;
  InvalidKey_Format;
  InvalidIPAddress_InUse;
  InvalidInternetGatewayID_NotFound;
  InvalidInterface_IpAddressLimitExceeded;
  InvalidInstanceType;
  InvalidInstanceID_NotLinkable;
  InvalidInstanceID_NotFound;
  InvalidInstanceID_Malformed;
  InvalidInstanceID;
  InvalidInstanceAttributeValue;
  InvalidInput;
  InvalidID;
  InvalidGroup_Reserved;
  InvalidGroup_NotFound;
  InvalidGroup_InUse;
  InvalidGroupId_Malformed;
  InvalidGroup_Duplicate;
  InvalidGatewayID_NotFound;
  InvalidFormat;
  InvalidFilter;
  InvalidExportTaskID_NotFound;
  InvalidDhcpOptionsId_Malformed;
  InvalidDhcpOptionsID_NotFound;
  InvalidDhcpOptionID_NotFound;
  InvalidDevice_InUse;
  InvalidCustomerGatewayID_NotFound;
  InvalidCustomerGatewayId_Malformed;
  InvalidCustomerGateway_DuplicateIpAddress;
  InvalidConversionTaskId;
  InvalidBundleID_NotFound;
  InvalidBlockDeviceMapping;
  InvalidAttachmentID_NotFound;
  InvalidAttachment_NotFound;
  InvalidAssociationID_NotFound;
  InvalidAMIName_Malformed;
  InvalidAMIName_Duplicate;
  InvalidAMIID_Unavailable;
  InvalidAMIID_NotFound;
  InvalidAMIID_Malformed;
  InvalidAMIAttributeItemValue;
  InvalidAllocationID_NotFound;
  InvalidAddressID_NotFound;
  InvalidAddress_NotFound;
  InternetGatewayLimitExceeded;
  InsufficientReservedInstancesCapacity;
  InsufficientFreeAddressesInSubnet;
  InstanceLimitExceeded;
  InstanceAlreadyLinked;
  IncorrectState;
  IncorrectInstanceState;
  Gateway_NotAttached;
  FilterLimitExceeded;
  EncryptedVolumesNotSupported;
  DiskImageSizeTooLarge;
  DependencyViolation;
  CustomerGatewayLimitExceeded;
  ConcurrentTagAccess;
  ConcurrentSnapshotLimitExceeded;
  CannotDelete;
  BundlingInProgress;
  AttachmentLimitExceeded;
  AddressLimitExceeded;
  ActiveVpcPeeringConnectionPerVpcLimitExceeded;
  UnsupportedProtocol;
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
  | ActiveVpcPeeringConnectionPerVpcLimitExceeded -> None
  | AddressLimitExceeded -> None
  | AttachmentLimitExceeded -> None
  | AuthFailure -> None
  | Blocked -> None
  | BundlingInProgress -> None
  | CannotDelete -> None
  | ConcurrentSnapshotLimitExceeded -> None
  | ConcurrentTagAccess -> None
  | CustomerGatewayLimitExceeded -> None
  | DependencyViolation -> None
  | DiskImageSizeTooLarge -> None
  | DryRunOperation -> None
  | EncryptedVolumesNotSupported -> None
  | FilterLimitExceeded -> None
  | Gateway_NotAttached -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | IncorrectInstanceState -> None
  | IncorrectState -> None
  | InstanceAlreadyLinked -> None
  | InstanceLimitExceeded -> None
  | InsufficientFreeAddressesInSubnet -> None
  | InsufficientReservedInstancesCapacity -> None
  | InternalFailure -> Some 500
  | InternetGatewayLimitExceeded -> None
  | InvalidAMIAttributeItemValue -> None
  | InvalidAMIID_Malformed -> None
  | InvalidAMIID_NotFound -> None
  | InvalidAMIID_Unavailable -> None
  | InvalidAMIName_Duplicate -> None
  | InvalidAMIName_Malformed -> None
  | InvalidAction -> Some 400
  | InvalidAddress_NotFound -> None
  | InvalidAddressID_NotFound -> None
  | InvalidAllocationID_NotFound -> None
  | InvalidAssociationID_NotFound -> None
  | InvalidAttachment_NotFound -> None
  | InvalidAttachmentID_NotFound -> None
  | InvalidBlockDeviceMapping -> None
  | InvalidBundleID_NotFound -> None
  | InvalidClientTokenId -> Some 403
  | InvalidConversionTaskId -> None
  | InvalidCustomerGateway_DuplicateIpAddress -> None
  | InvalidCustomerGatewayID_NotFound -> None
  | InvalidCustomerGatewayId_Malformed -> None
  | InvalidDevice_InUse -> None
  | InvalidDhcpOptionID_NotFound -> None
  | InvalidDhcpOptionsID_NotFound -> None
  | InvalidDhcpOptionsId_Malformed -> None
  | InvalidExportTaskID_NotFound -> None
  | InvalidFilter -> None
  | InvalidFormat -> None
  | InvalidGatewayID_NotFound -> None
  | InvalidGroup_Duplicate -> None
  | InvalidGroup_InUse -> None
  | InvalidGroup_NotFound -> None
  | InvalidGroup_Reserved -> None
  | InvalidGroupId_Malformed -> None
  | InvalidID -> None
  | InvalidIPAddress_InUse -> None
  | InvalidInput -> None
  | InvalidInstanceAttributeValue -> None
  | InvalidInstanceID -> None
  | InvalidInstanceID_Malformed -> None
  | InvalidInstanceID_NotFound -> None
  | InvalidInstanceID_NotLinkable -> None
  | InvalidInstanceType -> None
  | InvalidInterface_IpAddressLimitExceeded -> None
  | InvalidInternetGatewayID_NotFound -> None
  | InvalidKey_Format -> None
  | InvalidKeyPair_Duplicate -> None
  | InvalidKeyPair_Format -> None
  | InvalidKeyPair_NotFound -> None
  | InvalidManifest -> None
  | InvalidNetworkAclEntry_NotFound -> None
  | InvalidNetworkAclID_NotFound -> None
  | InvalidNetworkInterface_InUse -> None
  | InvalidNetworkInterfaceAttachmentID_Malformed -> None
  | InvalidNetworkInterfaceID_NotFound -> None
  | InvalidNetworkInterfaceId_Malformed -> None
  | InvalidOption_Conflict -> None
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidPermission_Duplicate -> None
  | InvalidPermission_Malformed -> None
  | InvalidPermission_NotFound -> None
  | InvalidPlacementGroup_Duplicate -> None
  | InvalidPlacementGroup_InUse -> None
  | InvalidPlacementGroup_Unknown -> None
  | InvalidQueryParameter -> Some 400
  | InvalidRequest -> None
  | InvalidReservationID_Malformed -> None
  | InvalidReservationID_NotFound -> None
  | InvalidReservedInstancesId -> None
  | InvalidReservedInstancesOfferingId -> None
  | InvalidRoute_Malformed -> None
  | InvalidRoute_NotFound -> None
  | InvalidRouteTableID_NotFound -> None
  | InvalidRouteTableId_Malformed -> None
  | InvalidSecurity_RequestHasExpired -> None
  | InvalidSecurityGroupID_NotFound -> None
  | InvalidSnapshot_InUse -> None
  | InvalidSnapshot_NotFound -> None
  | InvalidSnapshotID_Malformed -> None
  | InvalidSpotDatafeed_NotFound -> None
  | InvalidSpotInstanceRequestID_Malformed -> None
  | InvalidSpotInstanceRequestID_NotFound -> None
  | InvalidState -> None
  | InvalidStateTransition -> None
  | InvalidSubnet_Conflict -> None
  | InvalidSubnetID_NotFound -> None
  | InvalidUserID_Malformed -> None
  | InvalidVolume_NotFound -> None
  | InvalidVolume_ZoneMismatch -> None
  | InvalidVolumeID_Duplicate -> None
  | InvalidVolumeID_Malformed -> None
  | InvalidVolumeID_ZoneMismatch -> None
  | InvalidVpcID_NotFound -> None
  | InvalidVpcPeeringConnectionID_NotFound -> None
  | InvalidVpcPeeringConnectionId_Malformed -> None
  | InvalidVpcRange -> None
  | InvalidVpcState -> None
  | InvalidVpnConnectionID -> None
  | InvalidVpnConnectionID_NotFound -> None
  | InvalidVpnGatewayAttachment_NotFound -> None
  | InvalidVpnGatewayID_NotFound -> None
  | InvalidZone_NotFound -> None
  | LegacySecurityGroup -> None
  | MalformedQueryString -> Some 404
  | MaxIOPSLimitExceeded -> None
  | MaxSpotInstanceCountExceeded -> None
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NetworkAclEntryAlreadyExists -> None
  | NetworkAclEntryLimitExceeded -> None
  | NetworkAclLimitExceeded -> None
  | NonEBSInstance -> None
  | NotExportable -> None
  | OperationNotPermitted -> None
  | OptInRequired -> Some 403
  | OutstandingVpcPeeringConnectionLimitExceeded -> None
  | PendingSnapshotLimitExceeded -> None
  | PendingVerification -> None
  | PrivateIpAddressLimitExceeded -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | RequestResourceCountExceeded -> None
  | ReservedInstancesLimitExceeded -> None
  | Resource_AlreadyAssociated -> None
  | ResourceCountExceeded -> None
  | ResourceLimitExceeded -> None
  | RouteAlreadyExists -> None
  | RouteLimitExceeded -> None
  | RouteTableLimitExceeded -> None
  | RulesPerSecurityGroupLimitExceeded -> None
  | SecurityGroupLimitExceeded -> None
  | SecurityGroupsPerInstanceLimitExceeded -> None
  | SecurityGroupsPerInterfaceLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | SignatureDoesNotMatch -> None
  | SnapshotLimitExceeded -> None
  | SubnetLimitExceeded -> None
  | TagLimitExceeded -> None
  | Throttling -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnknownVolumeType -> None
  | Unsupported -> None
  | UnsupportedOperation -> None
  | UnsupportedProtocol -> None
  | VPCIdNotSpecified -> None
  | VPCResourceNotSpecified -> None
  | ValidationError -> Some 400
  | VolumeInUse -> None
  | VolumeLimitExceeded -> None
  | VolumeTypeNotAvailableInZone -> None
  | VpcCidrConflict -> None
  | VpcLimitExceeded -> None
  | VpcPeeringConnectionAlreadyExists -> None
  | VpnConnectionLimitExceeded -> None
  | VpnGatewayAttachmentLimitExceeded -> None
  | VpnGatewayLimitExceeded -> None
  | Uninhabited -> None
let to_string e =
  match e with
  | ActiveVpcPeeringConnectionPerVpcLimitExceeded ->
      "ActiveVpcPeeringConnectionPerVpcLimitExceeded"
  | AddressLimitExceeded -> "AddressLimitExceeded"
  | AttachmentLimitExceeded -> "AttachmentLimitExceeded"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | BundlingInProgress -> "BundlingInProgress"
  | CannotDelete -> "CannotDelete"
  | ConcurrentSnapshotLimitExceeded -> "ConcurrentSnapshotLimitExceeded"
  | ConcurrentTagAccess -> "ConcurrentTagAccess"
  | CustomerGatewayLimitExceeded -> "CustomerGatewayLimitExceeded"
  | DependencyViolation -> "DependencyViolation"
  | DiskImageSizeTooLarge -> "DiskImageSizeTooLarge"
  | DryRunOperation -> "DryRunOperation"
  | EncryptedVolumesNotSupported -> "EncryptedVolumesNotSupported"
  | FilterLimitExceeded -> "FilterLimitExceeded"
  | Gateway_NotAttached -> "Gateway.NotAttached"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | IncorrectInstanceState -> "IncorrectInstanceState"
  | IncorrectState -> "IncorrectState"
  | InstanceAlreadyLinked -> "InstanceAlreadyLinked"
  | InstanceLimitExceeded -> "InstanceLimitExceeded"
  | InsufficientFreeAddressesInSubnet -> "InsufficientFreeAddressesInSubnet"
  | InsufficientReservedInstancesCapacity ->
      "InsufficientReservedInstancesCapacity"
  | InternalFailure -> "InternalFailure"
  | InternetGatewayLimitExceeded -> "InternetGatewayLimitExceeded"
  | InvalidAMIAttributeItemValue -> "InvalidAMIAttributeItemValue"
  | InvalidAMIID_Malformed -> "InvalidAMIID.Malformed"
  | InvalidAMIID_NotFound -> "InvalidAMIID.NotFound"
  | InvalidAMIID_Unavailable -> "InvalidAMIID.Unavailable"
  | InvalidAMIName_Duplicate -> "InvalidAMIName.Duplicate"
  | InvalidAMIName_Malformed -> "InvalidAMIName.Malformed"
  | InvalidAction -> "InvalidAction"
  | InvalidAddress_NotFound -> "InvalidAddress.NotFound"
  | InvalidAddressID_NotFound -> "InvalidAddressID.NotFound"
  | InvalidAllocationID_NotFound -> "InvalidAllocationID.NotFound"
  | InvalidAssociationID_NotFound -> "InvalidAssociationID.NotFound"
  | InvalidAttachment_NotFound -> "InvalidAttachment.NotFound"
  | InvalidAttachmentID_NotFound -> "InvalidAttachmentID.NotFound"
  | InvalidBlockDeviceMapping -> "InvalidBlockDeviceMapping"
  | InvalidBundleID_NotFound -> "InvalidBundleID.NotFound"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidConversionTaskId -> "InvalidConversionTaskId"
  | InvalidCustomerGateway_DuplicateIpAddress ->
      "InvalidCustomerGateway.DuplicateIpAddress"
  | InvalidCustomerGatewayID_NotFound -> "InvalidCustomerGatewayID.NotFound"
  | InvalidCustomerGatewayId_Malformed ->
      "InvalidCustomerGatewayId.Malformed"
  | InvalidDevice_InUse -> "InvalidDevice.InUse"
  | InvalidDhcpOptionID_NotFound -> "InvalidDhcpOptionID.NotFound"
  | InvalidDhcpOptionsID_NotFound -> "InvalidDhcpOptionsID.NotFound"
  | InvalidDhcpOptionsId_Malformed -> "InvalidDhcpOptionsId.Malformed"
  | InvalidExportTaskID_NotFound -> "InvalidExportTaskID.NotFound"
  | InvalidFilter -> "InvalidFilter"
  | InvalidFormat -> "InvalidFormat"
  | InvalidGatewayID_NotFound -> "InvalidGatewayID.NotFound"
  | InvalidGroup_Duplicate -> "InvalidGroup.Duplicate"
  | InvalidGroup_InUse -> "InvalidGroup.InUse"
  | InvalidGroup_NotFound -> "InvalidGroup.NotFound"
  | InvalidGroup_Reserved -> "InvalidGroup.Reserved"
  | InvalidGroupId_Malformed -> "InvalidGroupId.Malformed"
  | InvalidID -> "InvalidID"
  | InvalidIPAddress_InUse -> "InvalidIPAddress.InUse"
  | InvalidInput -> "InvalidInput"
  | InvalidInstanceAttributeValue -> "InvalidInstanceAttributeValue"
  | InvalidInstanceID -> "InvalidInstanceID"
  | InvalidInstanceID_Malformed -> "InvalidInstanceID.Malformed"
  | InvalidInstanceID_NotFound -> "InvalidInstanceID.NotFound"
  | InvalidInstanceID_NotLinkable -> "InvalidInstanceID.NotLinkable"
  | InvalidInstanceType -> "InvalidInstanceType"
  | InvalidInterface_IpAddressLimitExceeded ->
      "InvalidInterface.IpAddressLimitExceeded"
  | InvalidInternetGatewayID_NotFound -> "InvalidInternetGatewayID.NotFound"
  | InvalidKey_Format -> "InvalidKey.Format"
  | InvalidKeyPair_Duplicate -> "InvalidKeyPair.Duplicate"
  | InvalidKeyPair_Format -> "InvalidKeyPair.Format"
  | InvalidKeyPair_NotFound -> "InvalidKeyPair.NotFound"
  | InvalidManifest -> "InvalidManifest"
  | InvalidNetworkAclEntry_NotFound -> "InvalidNetworkAclEntry.NotFound"
  | InvalidNetworkAclID_NotFound -> "InvalidNetworkAclID.NotFound"
  | InvalidNetworkInterface_InUse -> "InvalidNetworkInterface.InUse"
  | InvalidNetworkInterfaceAttachmentID_Malformed ->
      "InvalidNetworkInterfaceAttachmentID.Malformed"
  | InvalidNetworkInterfaceID_NotFound ->
      "InvalidNetworkInterfaceID.NotFound"
  | InvalidNetworkInterfaceId_Malformed ->
      "InvalidNetworkInterfaceId.Malformed"
  | InvalidOption_Conflict -> "InvalidOption.Conflict"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidPermission_Duplicate -> "InvalidPermission.Duplicate"
  | InvalidPermission_Malformed -> "InvalidPermission.Malformed"
  | InvalidPermission_NotFound -> "InvalidPermission.NotFound"
  | InvalidPlacementGroup_Duplicate -> "InvalidPlacementGroup.Duplicate"
  | InvalidPlacementGroup_InUse -> "InvalidPlacementGroup.InUse"
  | InvalidPlacementGroup_Unknown -> "InvalidPlacementGroup.Unknown"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | InvalidRequest -> "InvalidRequest"
  | InvalidReservationID_Malformed -> "InvalidReservationID.Malformed"
  | InvalidReservationID_NotFound -> "InvalidReservationID.NotFound"
  | InvalidReservedInstancesId -> "InvalidReservedInstancesId"
  | InvalidReservedInstancesOfferingId ->
      "InvalidReservedInstancesOfferingId"
  | InvalidRoute_Malformed -> "InvalidRoute.Malformed"
  | InvalidRoute_NotFound -> "InvalidRoute.NotFound"
  | InvalidRouteTableID_NotFound -> "InvalidRouteTableID.NotFound"
  | InvalidRouteTableId_Malformed -> "InvalidRouteTableId.Malformed"
  | InvalidSecurity_RequestHasExpired -> "InvalidSecurity.RequestHasExpired"
  | InvalidSecurityGroupID_NotFound -> "InvalidSecurityGroupID.NotFound"
  | InvalidSnapshot_InUse -> "InvalidSnapshot.InUse"
  | InvalidSnapshot_NotFound -> "InvalidSnapshot.NotFound"
  | InvalidSnapshotID_Malformed -> "InvalidSnapshotID.Malformed"
  | InvalidSpotDatafeed_NotFound -> "InvalidSpotDatafeed.NotFound"
  | InvalidSpotInstanceRequestID_Malformed ->
      "InvalidSpotInstanceRequestID.Malformed"
  | InvalidSpotInstanceRequestID_NotFound ->
      "InvalidSpotInstanceRequestID.NotFound"
  | InvalidState -> "InvalidState"
  | InvalidStateTransition -> "InvalidStateTransition"
  | InvalidSubnet_Conflict -> "InvalidSubnet.Conflict"
  | InvalidSubnetID_NotFound -> "InvalidSubnetID.NotFound"
  | InvalidUserID_Malformed -> "InvalidUserID.Malformed"
  | InvalidVolume_NotFound -> "InvalidVolume.NotFound"
  | InvalidVolume_ZoneMismatch -> "InvalidVolume.ZoneMismatch"
  | InvalidVolumeID_Duplicate -> "InvalidVolumeID.Duplicate"
  | InvalidVolumeID_Malformed -> "InvalidVolumeID.Malformed"
  | InvalidVolumeID_ZoneMismatch -> "InvalidVolumeID.ZoneMismatch"
  | InvalidVpcID_NotFound -> "InvalidVpcID.NotFound"
  | InvalidVpcPeeringConnectionID_NotFound ->
      "InvalidVpcPeeringConnectionID.NotFound"
  | InvalidVpcPeeringConnectionId_Malformed ->
      "InvalidVpcPeeringConnectionId.Malformed"
  | InvalidVpcRange -> "InvalidVpcRange"
  | InvalidVpcState -> "InvalidVpcState"
  | InvalidVpnConnectionID -> "InvalidVpnConnectionID"
  | InvalidVpnConnectionID_NotFound -> "InvalidVpnConnectionID.NotFound"
  | InvalidVpnGatewayAttachment_NotFound ->
      "InvalidVpnGatewayAttachment.NotFound"
  | InvalidVpnGatewayID_NotFound -> "InvalidVpnGatewayID.NotFound"
  | InvalidZone_NotFound -> "InvalidZone.NotFound"
  | LegacySecurityGroup -> "LegacySecurityGroup"
  | MalformedQueryString -> "MalformedQueryString"
  | MaxIOPSLimitExceeded -> "MaxIOPSLimitExceeded"
  | MaxSpotInstanceCountExceeded -> "MaxSpotInstanceCountExceeded"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NetworkAclEntryAlreadyExists -> "NetworkAclEntryAlreadyExists"
  | NetworkAclEntryLimitExceeded -> "NetworkAclEntryLimitExceeded"
  | NetworkAclLimitExceeded -> "NetworkAclLimitExceeded"
  | NonEBSInstance -> "NonEBSInstance"
  | NotExportable -> "NotExportable"
  | OperationNotPermitted -> "OperationNotPermitted"
  | OptInRequired -> "OptInRequired"
  | OutstandingVpcPeeringConnectionLimitExceeded ->
      "OutstandingVpcPeeringConnectionLimitExceeded"
  | PendingSnapshotLimitExceeded -> "PendingSnapshotLimitExceeded"
  | PendingVerification -> "PendingVerification"
  | PrivateIpAddressLimitExceeded -> "PrivateIpAddressLimitExceeded"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | RequestResourceCountExceeded -> "RequestResourceCountExceeded"
  | ReservedInstancesLimitExceeded -> "ReservedInstancesLimitExceeded"
  | Resource_AlreadyAssociated -> "Resource.AlreadyAssociated"
  | ResourceCountExceeded -> "ResourceCountExceeded"
  | ResourceLimitExceeded -> "ResourceLimitExceeded"
  | RouteAlreadyExists -> "RouteAlreadyExists"
  | RouteLimitExceeded -> "RouteLimitExceeded"
  | RouteTableLimitExceeded -> "RouteTableLimitExceeded"
  | RulesPerSecurityGroupLimitExceeded ->
      "RulesPerSecurityGroupLimitExceeded"
  | SecurityGroupLimitExceeded -> "SecurityGroupLimitExceeded"
  | SecurityGroupsPerInstanceLimitExceeded ->
      "SecurityGroupsPerInstanceLimitExceeded"
  | SecurityGroupsPerInterfaceLimitExceeded ->
      "SecurityGroupsPerInterfaceLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | SignatureDoesNotMatch -> "SignatureDoesNotMatch"
  | SnapshotLimitExceeded -> "SnapshotLimitExceeded"
  | SubnetLimitExceeded -> "SubnetLimitExceeded"
  | TagLimitExceeded -> "TagLimitExceeded"
  | Throttling -> "Throttling"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnknownVolumeType -> "UnknownVolumeType"
  | Unsupported -> "Unsupported"
  | UnsupportedOperation -> "UnsupportedOperation"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | VPCIdNotSpecified -> "VPCIdNotSpecified"
  | VPCResourceNotSpecified -> "VPCResourceNotSpecified"
  | ValidationError -> "ValidationError"
  | VolumeInUse -> "VolumeInUse"
  | VolumeLimitExceeded -> "VolumeLimitExceeded"
  | VolumeTypeNotAvailableInZone -> "VolumeTypeNotAvailableInZone"
  | VpcCidrConflict -> "VpcCidrConflict"
  | VpcLimitExceeded -> "VpcLimitExceeded"
  | VpcPeeringConnectionAlreadyExists -> "VpcPeeringConnectionAlreadyExists"
  | VpnConnectionLimitExceeded -> "VpnConnectionLimitExceeded"
  | VpnGatewayAttachmentLimitExceeded -> "VpnGatewayAttachmentLimitExceeded"
  | VpnGatewayLimitExceeded -> "VpnGatewayLimitExceeded"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "ActiveVpcPeeringConnectionPerVpcLimitExceeded" ->
      Some ActiveVpcPeeringConnectionPerVpcLimitExceeded
  | "AddressLimitExceeded" -> Some AddressLimitExceeded
  | "AttachmentLimitExceeded" -> Some AttachmentLimitExceeded
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "BundlingInProgress" -> Some BundlingInProgress
  | "CannotDelete" -> Some CannotDelete
  | "ConcurrentSnapshotLimitExceeded" -> Some ConcurrentSnapshotLimitExceeded
  | "ConcurrentTagAccess" -> Some ConcurrentTagAccess
  | "CustomerGatewayLimitExceeded" -> Some CustomerGatewayLimitExceeded
  | "DependencyViolation" -> Some DependencyViolation
  | "DiskImageSizeTooLarge" -> Some DiskImageSizeTooLarge
  | "DryRunOperation" -> Some DryRunOperation
  | "EncryptedVolumesNotSupported" -> Some EncryptedVolumesNotSupported
  | "FilterLimitExceeded" -> Some FilterLimitExceeded
  | "Gateway.NotAttached" -> Some Gateway_NotAttached
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "IncorrectInstanceState" -> Some IncorrectInstanceState
  | "IncorrectState" -> Some IncorrectState
  | "InstanceAlreadyLinked" -> Some InstanceAlreadyLinked
  | "InstanceLimitExceeded" -> Some InstanceLimitExceeded
  | "InsufficientFreeAddressesInSubnet" ->
      Some InsufficientFreeAddressesInSubnet
  | "InsufficientReservedInstancesCapacity" ->
      Some InsufficientReservedInstancesCapacity
  | "InternalFailure" -> Some InternalFailure
  | "InternetGatewayLimitExceeded" -> Some InternetGatewayLimitExceeded
  | "InvalidAMIAttributeItemValue" -> Some InvalidAMIAttributeItemValue
  | "InvalidAMIID.Malformed" -> Some InvalidAMIID_Malformed
  | "InvalidAMIID.NotFound" -> Some InvalidAMIID_NotFound
  | "InvalidAMIID.Unavailable" -> Some InvalidAMIID_Unavailable
  | "InvalidAMIName.Duplicate" -> Some InvalidAMIName_Duplicate
  | "InvalidAMIName.Malformed" -> Some InvalidAMIName_Malformed
  | "InvalidAction" -> Some InvalidAction
  | "InvalidAddress.NotFound" -> Some InvalidAddress_NotFound
  | "InvalidAddressID.NotFound" -> Some InvalidAddressID_NotFound
  | "InvalidAllocationID.NotFound" -> Some InvalidAllocationID_NotFound
  | "InvalidAssociationID.NotFound" -> Some InvalidAssociationID_NotFound
  | "InvalidAttachment.NotFound" -> Some InvalidAttachment_NotFound
  | "InvalidAttachmentID.NotFound" -> Some InvalidAttachmentID_NotFound
  | "InvalidBlockDeviceMapping" -> Some InvalidBlockDeviceMapping
  | "InvalidBundleID.NotFound" -> Some InvalidBundleID_NotFound
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidConversionTaskId" -> Some InvalidConversionTaskId
  | "InvalidCustomerGateway.DuplicateIpAddress" ->
      Some InvalidCustomerGateway_DuplicateIpAddress
  | "InvalidCustomerGatewayID.NotFound" ->
      Some InvalidCustomerGatewayID_NotFound
  | "InvalidCustomerGatewayId.Malformed" ->
      Some InvalidCustomerGatewayId_Malformed
  | "InvalidDevice.InUse" -> Some InvalidDevice_InUse
  | "InvalidDhcpOptionID.NotFound" -> Some InvalidDhcpOptionID_NotFound
  | "InvalidDhcpOptionsID.NotFound" -> Some InvalidDhcpOptionsID_NotFound
  | "InvalidDhcpOptionsId.Malformed" -> Some InvalidDhcpOptionsId_Malformed
  | "InvalidExportTaskID.NotFound" -> Some InvalidExportTaskID_NotFound
  | "InvalidFilter" -> Some InvalidFilter
  | "InvalidFormat" -> Some InvalidFormat
  | "InvalidGatewayID.NotFound" -> Some InvalidGatewayID_NotFound
  | "InvalidGroup.Duplicate" -> Some InvalidGroup_Duplicate
  | "InvalidGroup.InUse" -> Some InvalidGroup_InUse
  | "InvalidGroup.NotFound" -> Some InvalidGroup_NotFound
  | "InvalidGroup.Reserved" -> Some InvalidGroup_Reserved
  | "InvalidGroupId.Malformed" -> Some InvalidGroupId_Malformed
  | "InvalidID" -> Some InvalidID
  | "InvalidIPAddress.InUse" -> Some InvalidIPAddress_InUse
  | "InvalidInput" -> Some InvalidInput
  | "InvalidInstanceAttributeValue" -> Some InvalidInstanceAttributeValue
  | "InvalidInstanceID" -> Some InvalidInstanceID
  | "InvalidInstanceID.Malformed" -> Some InvalidInstanceID_Malformed
  | "InvalidInstanceID.NotFound" -> Some InvalidInstanceID_NotFound
  | "InvalidInstanceID.NotLinkable" -> Some InvalidInstanceID_NotLinkable
  | "InvalidInstanceType" -> Some InvalidInstanceType
  | "InvalidInterface.IpAddressLimitExceeded" ->
      Some InvalidInterface_IpAddressLimitExceeded
  | "InvalidInternetGatewayID.NotFound" ->
      Some InvalidInternetGatewayID_NotFound
  | "InvalidKey.Format" -> Some InvalidKey_Format
  | "InvalidKeyPair.Duplicate" -> Some InvalidKeyPair_Duplicate
  | "InvalidKeyPair.Format" -> Some InvalidKeyPair_Format
  | "InvalidKeyPair.NotFound" -> Some InvalidKeyPair_NotFound
  | "InvalidManifest" -> Some InvalidManifest
  | "InvalidNetworkAclEntry.NotFound" -> Some InvalidNetworkAclEntry_NotFound
  | "InvalidNetworkAclID.NotFound" -> Some InvalidNetworkAclID_NotFound
  | "InvalidNetworkInterface.InUse" -> Some InvalidNetworkInterface_InUse
  | "InvalidNetworkInterfaceAttachmentID.Malformed" ->
      Some InvalidNetworkInterfaceAttachmentID_Malformed
  | "InvalidNetworkInterfaceID.NotFound" ->
      Some InvalidNetworkInterfaceID_NotFound
  | "InvalidNetworkInterfaceId.Malformed" ->
      Some InvalidNetworkInterfaceId_Malformed
  | "InvalidOption.Conflict" -> Some InvalidOption_Conflict
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidPermission.Duplicate" -> Some InvalidPermission_Duplicate
  | "InvalidPermission.Malformed" -> Some InvalidPermission_Malformed
  | "InvalidPermission.NotFound" -> Some InvalidPermission_NotFound
  | "InvalidPlacementGroup.Duplicate" -> Some InvalidPlacementGroup_Duplicate
  | "InvalidPlacementGroup.InUse" -> Some InvalidPlacementGroup_InUse
  | "InvalidPlacementGroup.Unknown" -> Some InvalidPlacementGroup_Unknown
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "InvalidRequest" -> Some InvalidRequest
  | "InvalidReservationID.Malformed" -> Some InvalidReservationID_Malformed
  | "InvalidReservationID.NotFound" -> Some InvalidReservationID_NotFound
  | "InvalidReservedInstancesId" -> Some InvalidReservedInstancesId
  | "InvalidReservedInstancesOfferingId" ->
      Some InvalidReservedInstancesOfferingId
  | "InvalidRoute.Malformed" -> Some InvalidRoute_Malformed
  | "InvalidRoute.NotFound" -> Some InvalidRoute_NotFound
  | "InvalidRouteTableID.NotFound" -> Some InvalidRouteTableID_NotFound
  | "InvalidRouteTableId.Malformed" -> Some InvalidRouteTableId_Malformed
  | "InvalidSecurity.RequestHasExpired" ->
      Some InvalidSecurity_RequestHasExpired
  | "InvalidSecurityGroupID.NotFound" -> Some InvalidSecurityGroupID_NotFound
  | "InvalidSnapshot.InUse" -> Some InvalidSnapshot_InUse
  | "InvalidSnapshot.NotFound" -> Some InvalidSnapshot_NotFound
  | "InvalidSnapshotID.Malformed" -> Some InvalidSnapshotID_Malformed
  | "InvalidSpotDatafeed.NotFound" -> Some InvalidSpotDatafeed_NotFound
  | "InvalidSpotInstanceRequestID.Malformed" ->
      Some InvalidSpotInstanceRequestID_Malformed
  | "InvalidSpotInstanceRequestID.NotFound" ->
      Some InvalidSpotInstanceRequestID_NotFound
  | "InvalidState" -> Some InvalidState
  | "InvalidStateTransition" -> Some InvalidStateTransition
  | "InvalidSubnet.Conflict" -> Some InvalidSubnet_Conflict
  | "InvalidSubnetID.NotFound" -> Some InvalidSubnetID_NotFound
  | "InvalidUserID.Malformed" -> Some InvalidUserID_Malformed
  | "InvalidVolume.NotFound" -> Some InvalidVolume_NotFound
  | "InvalidVolume.ZoneMismatch" -> Some InvalidVolume_ZoneMismatch
  | "InvalidVolumeID.Duplicate" -> Some InvalidVolumeID_Duplicate
  | "InvalidVolumeID.Malformed" -> Some InvalidVolumeID_Malformed
  | "InvalidVolumeID.ZoneMismatch" -> Some InvalidVolumeID_ZoneMismatch
  | "InvalidVpcID.NotFound" -> Some InvalidVpcID_NotFound
  | "InvalidVpcPeeringConnectionID.NotFound" ->
      Some InvalidVpcPeeringConnectionID_NotFound
  | "InvalidVpcPeeringConnectionId.Malformed" ->
      Some InvalidVpcPeeringConnectionId_Malformed
  | "InvalidVpcRange" -> Some InvalidVpcRange
  | "InvalidVpcState" -> Some InvalidVpcState
  | "InvalidVpnConnectionID" -> Some InvalidVpnConnectionID
  | "InvalidVpnConnectionID.NotFound" -> Some InvalidVpnConnectionID_NotFound
  | "InvalidVpnGatewayAttachment.NotFound" ->
      Some InvalidVpnGatewayAttachment_NotFound
  | "InvalidVpnGatewayID.NotFound" -> Some InvalidVpnGatewayID_NotFound
  | "InvalidZone.NotFound" -> Some InvalidZone_NotFound
  | "LegacySecurityGroup" -> Some LegacySecurityGroup
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MaxIOPSLimitExceeded" -> Some MaxIOPSLimitExceeded
  | "MaxSpotInstanceCountExceeded" -> Some MaxSpotInstanceCountExceeded
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NetworkAclEntryAlreadyExists" -> Some NetworkAclEntryAlreadyExists
  | "NetworkAclEntryLimitExceeded" -> Some NetworkAclEntryLimitExceeded
  | "NetworkAclLimitExceeded" -> Some NetworkAclLimitExceeded
  | "NonEBSInstance" -> Some NonEBSInstance
  | "NotExportable" -> Some NotExportable
  | "OperationNotPermitted" -> Some OperationNotPermitted
  | "OptInRequired" -> Some OptInRequired
  | "OutstandingVpcPeeringConnectionLimitExceeded" ->
      Some OutstandingVpcPeeringConnectionLimitExceeded
  | "PendingSnapshotLimitExceeded" -> Some PendingSnapshotLimitExceeded
  | "PendingVerification" -> Some PendingVerification
  | "PrivateIpAddressLimitExceeded" -> Some PrivateIpAddressLimitExceeded
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "RequestResourceCountExceeded" -> Some RequestResourceCountExceeded
  | "ReservedInstancesLimitExceeded" -> Some ReservedInstancesLimitExceeded
  | "Resource.AlreadyAssociated" -> Some Resource_AlreadyAssociated
  | "ResourceCountExceeded" -> Some ResourceCountExceeded
  | "ResourceLimitExceeded" -> Some ResourceLimitExceeded
  | "RouteAlreadyExists" -> Some RouteAlreadyExists
  | "RouteLimitExceeded" -> Some RouteLimitExceeded
  | "RouteTableLimitExceeded" -> Some RouteTableLimitExceeded
  | "RulesPerSecurityGroupLimitExceeded" ->
      Some RulesPerSecurityGroupLimitExceeded
  | "SecurityGroupLimitExceeded" -> Some SecurityGroupLimitExceeded
  | "SecurityGroupsPerInstanceLimitExceeded" ->
      Some SecurityGroupsPerInstanceLimitExceeded
  | "SecurityGroupsPerInterfaceLimitExceeded" ->
      Some SecurityGroupsPerInterfaceLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "SignatureDoesNotMatch" -> Some SignatureDoesNotMatch
  | "SnapshotLimitExceeded" -> Some SnapshotLimitExceeded
  | "SubnetLimitExceeded" -> Some SubnetLimitExceeded
  | "TagLimitExceeded" -> Some TagLimitExceeded
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnknownVolumeType" -> Some UnknownVolumeType
  | "Unsupported" -> Some Unsupported
  | "UnsupportedOperation" -> Some UnsupportedOperation
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "VPCIdNotSpecified" -> Some VPCIdNotSpecified
  | "VPCResourceNotSpecified" -> Some VPCResourceNotSpecified
  | "ValidationError" -> Some ValidationError
  | "VolumeInUse" -> Some VolumeInUse
  | "VolumeLimitExceeded" -> Some VolumeLimitExceeded
  | "VolumeTypeNotAvailableInZone" -> Some VolumeTypeNotAvailableInZone
  | "VpcCidrConflict" -> Some VpcCidrConflict
  | "VpcLimitExceeded" -> Some VpcLimitExceeded
  | "VpcPeeringConnectionAlreadyExists" ->
      Some VpcPeeringConnectionAlreadyExists
  | "VpnConnectionLimitExceeded" -> Some VpnConnectionLimitExceeded
  | "VpnGatewayAttachmentLimitExceeded" ->
      Some VpnGatewayAttachmentLimitExceeded
  | "VpnGatewayLimitExceeded" -> Some VpnGatewayLimitExceeded
  | "Uninhabited" -> Some Uninhabited
  | _ -> None