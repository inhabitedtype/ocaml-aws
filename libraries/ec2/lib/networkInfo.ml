open Aws.BaseTypes

type t =
  { network_performance : String.t option
  ; maximum_network_interfaces : Integer.t option
  ; maximum_network_cards : Integer.t option
  ; default_network_card_index : Integer.t option
  ; network_cards : NetworkCardInfoList.t
  ; ipv4_addresses_per_interface : Integer.t option
  ; ipv6_addresses_per_interface : Integer.t option
  ; ipv6_supported : Boolean.t option
  ; ena_support : EnaSupport.t option
  ; efa_supported : Boolean.t option
  ; efa_info : EfaInfo.t option
  ; encryption_in_transit_supported : Boolean.t option
  ; ena_srd_supported : Boolean.t option
  ; bandwidth_weightings : BandwidthWeightingTypeList.t
  ; flexible_ena_queues_support : FlexibleEnaQueuesSupport.t option
  ; connection_tracking_configuration : DefaultConnectionTrackingConfiguration.t option
  ; secondary_network_supported : Boolean.t option
  ; maximum_secondary_network_interfaces : Integer.t option
  ; ipv4_addresses_per_secondary_interface : Integer.t option
  }

let make
    ?network_performance
    ?maximum_network_interfaces
    ?maximum_network_cards
    ?default_network_card_index
    ?(network_cards = [])
    ?ipv4_addresses_per_interface
    ?ipv6_addresses_per_interface
    ?ipv6_supported
    ?ena_support
    ?efa_supported
    ?efa_info
    ?encryption_in_transit_supported
    ?ena_srd_supported
    ?(bandwidth_weightings = [])
    ?flexible_ena_queues_support
    ?connection_tracking_configuration
    ?secondary_network_supported
    ?maximum_secondary_network_interfaces
    ?ipv4_addresses_per_secondary_interface
    () =
  { network_performance
  ; maximum_network_interfaces
  ; maximum_network_cards
  ; default_network_card_index
  ; network_cards
  ; ipv4_addresses_per_interface
  ; ipv6_addresses_per_interface
  ; ipv6_supported
  ; ena_support
  ; efa_supported
  ; efa_info
  ; encryption_in_transit_supported
  ; ena_srd_supported
  ; bandwidth_weightings
  ; flexible_ena_queues_support
  ; connection_tracking_configuration
  ; secondary_network_supported
  ; maximum_secondary_network_interfaces
  ; ipv4_addresses_per_secondary_interface
  }

let parse xml =
  Some
    { network_performance =
        Aws.Util.option_bind (Aws.Xml.member "networkPerformance" xml) String.parse
    ; maximum_network_interfaces =
        Aws.Util.option_bind (Aws.Xml.member "maximumNetworkInterfaces" xml) Integer.parse
    ; maximum_network_cards =
        Aws.Util.option_bind (Aws.Xml.member "maximumNetworkCards" xml) Integer.parse
    ; default_network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "defaultNetworkCardIndex" xml) Integer.parse
    ; network_cards =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkCards" xml)
             NetworkCardInfoList.parse)
    ; ipv4_addresses_per_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv4AddressesPerInterface" xml)
          Integer.parse
    ; ipv6_addresses_per_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv6AddressesPerInterface" xml)
          Integer.parse
    ; ipv6_supported =
        Aws.Util.option_bind (Aws.Xml.member "ipv6Supported" xml) Boolean.parse
    ; ena_support =
        Aws.Util.option_bind (Aws.Xml.member "enaSupport" xml) EnaSupport.parse
    ; efa_supported =
        Aws.Util.option_bind (Aws.Xml.member "efaSupported" xml) Boolean.parse
    ; efa_info = Aws.Util.option_bind (Aws.Xml.member "efaInfo" xml) EfaInfo.parse
    ; encryption_in_transit_supported =
        Aws.Util.option_bind
          (Aws.Xml.member "encryptionInTransitSupported" xml)
          Boolean.parse
    ; ena_srd_supported =
        Aws.Util.option_bind (Aws.Xml.member "enaSrdSupported" xml) Boolean.parse
    ; bandwidth_weightings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "bandwidthWeightings" xml)
             BandwidthWeightingTypeList.parse)
    ; flexible_ena_queues_support =
        Aws.Util.option_bind
          (Aws.Xml.member "flexibleEnaQueuesSupport" xml)
          FlexibleEnaQueuesSupport.parse
    ; connection_tracking_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionTrackingConfiguration" xml)
          DefaultConnectionTrackingConfiguration.parse
    ; secondary_network_supported =
        Aws.Util.option_bind
          (Aws.Xml.member "secondaryNetworkSupported" xml)
          Boolean.parse
    ; maximum_secondary_network_interfaces =
        Aws.Util.option_bind
          (Aws.Xml.member "maximumSecondaryNetworkInterfaces" xml)
          Integer.parse
    ; ipv4_addresses_per_secondary_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv4AddressesPerSecondaryInterface" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv4_addresses_per_secondary_interface (fun f ->
             Aws.Query.Pair ("Ipv4AddressesPerSecondaryInterface", Integer.to_query f))
       ; Aws.Util.option_map v.maximum_secondary_network_interfaces (fun f ->
             Aws.Query.Pair ("MaximumSecondaryNetworkInterfaces", Integer.to_query f))
       ; Aws.Util.option_map v.secondary_network_supported (fun f ->
             Aws.Query.Pair ("SecondaryNetworkSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.connection_tracking_configuration (fun f ->
             Aws.Query.Pair
               ( "ConnectionTrackingConfiguration"
               , DefaultConnectionTrackingConfiguration.to_query f ))
       ; Aws.Util.option_map v.flexible_ena_queues_support (fun f ->
             Aws.Query.Pair
               ("FlexibleEnaQueuesSupport", FlexibleEnaQueuesSupport.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BandwidthWeightings"
              , BandwidthWeightingTypeList.to_query v.bandwidth_weightings ))
       ; Aws.Util.option_map v.ena_srd_supported (fun f ->
             Aws.Query.Pair ("EnaSrdSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.encryption_in_transit_supported (fun f ->
             Aws.Query.Pair ("EncryptionInTransitSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.efa_info (fun f ->
             Aws.Query.Pair ("EfaInfo", EfaInfo.to_query f))
       ; Aws.Util.option_map v.efa_supported (fun f ->
             Aws.Query.Pair ("EfaSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.ena_support (fun f ->
             Aws.Query.Pair ("EnaSupport", EnaSupport.to_query f))
       ; Aws.Util.option_map v.ipv6_supported (fun f ->
             Aws.Query.Pair ("Ipv6Supported", Boolean.to_query f))
       ; Aws.Util.option_map v.ipv6_addresses_per_interface (fun f ->
             Aws.Query.Pair ("Ipv6AddressesPerInterface", Integer.to_query f))
       ; Aws.Util.option_map v.ipv4_addresses_per_interface (fun f ->
             Aws.Query.Pair ("Ipv4AddressesPerInterface", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("NetworkCards", NetworkCardInfoList.to_query v.network_cards))
       ; Aws.Util.option_map v.default_network_card_index (fun f ->
             Aws.Query.Pair ("DefaultNetworkCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.maximum_network_cards (fun f ->
             Aws.Query.Pair ("MaximumNetworkCards", Integer.to_query f))
       ; Aws.Util.option_map v.maximum_network_interfaces (fun f ->
             Aws.Query.Pair ("MaximumNetworkInterfaces", Integer.to_query f))
       ; Aws.Util.option_map v.network_performance (fun f ->
             Aws.Query.Pair ("NetworkPerformance", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv4_addresses_per_secondary_interface (fun f ->
             "ipv4AddressesPerSecondaryInterface", Integer.to_json f)
       ; Aws.Util.option_map v.maximum_secondary_network_interfaces (fun f ->
             "maximumSecondaryNetworkInterfaces", Integer.to_json f)
       ; Aws.Util.option_map v.secondary_network_supported (fun f ->
             "secondaryNetworkSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.connection_tracking_configuration (fun f ->
             ( "connectionTrackingConfiguration"
             , DefaultConnectionTrackingConfiguration.to_json f ))
       ; Aws.Util.option_map v.flexible_ena_queues_support (fun f ->
             "flexibleEnaQueuesSupport", FlexibleEnaQueuesSupport.to_json f)
       ; Some
           ( "bandwidthWeightings"
           , BandwidthWeightingTypeList.to_json v.bandwidth_weightings )
       ; Aws.Util.option_map v.ena_srd_supported (fun f ->
             "enaSrdSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.encryption_in_transit_supported (fun f ->
             "encryptionInTransitSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.efa_info (fun f -> "efaInfo", EfaInfo.to_json f)
       ; Aws.Util.option_map v.efa_supported (fun f -> "efaSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.ena_support (fun f -> "enaSupport", EnaSupport.to_json f)
       ; Aws.Util.option_map v.ipv6_supported (fun f ->
             "ipv6Supported", Boolean.to_json f)
       ; Aws.Util.option_map v.ipv6_addresses_per_interface (fun f ->
             "ipv6AddressesPerInterface", Integer.to_json f)
       ; Aws.Util.option_map v.ipv4_addresses_per_interface (fun f ->
             "ipv4AddressesPerInterface", Integer.to_json f)
       ; Some ("networkCards", NetworkCardInfoList.to_json v.network_cards)
       ; Aws.Util.option_map v.default_network_card_index (fun f ->
             "defaultNetworkCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.maximum_network_cards (fun f ->
             "maximumNetworkCards", Integer.to_json f)
       ; Aws.Util.option_map v.maximum_network_interfaces (fun f ->
             "maximumNetworkInterfaces", Integer.to_json f)
       ; Aws.Util.option_map v.network_performance (fun f ->
             "networkPerformance", String.to_json f)
       ])

let of_json j =
  { network_performance =
      Aws.Util.option_map (Aws.Json.lookup j "networkPerformance") String.of_json
  ; maximum_network_interfaces =
      Aws.Util.option_map (Aws.Json.lookup j "maximumNetworkInterfaces") Integer.of_json
  ; maximum_network_cards =
      Aws.Util.option_map (Aws.Json.lookup j "maximumNetworkCards") Integer.of_json
  ; default_network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "defaultNetworkCardIndex") Integer.of_json
  ; network_cards =
      NetworkCardInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkCards"))
  ; ipv4_addresses_per_interface =
      Aws.Util.option_map (Aws.Json.lookup j "ipv4AddressesPerInterface") Integer.of_json
  ; ipv6_addresses_per_interface =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6AddressesPerInterface") Integer.of_json
  ; ipv6_supported =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6Supported") Boolean.of_json
  ; ena_support = Aws.Util.option_map (Aws.Json.lookup j "enaSupport") EnaSupport.of_json
  ; efa_supported = Aws.Util.option_map (Aws.Json.lookup j "efaSupported") Boolean.of_json
  ; efa_info = Aws.Util.option_map (Aws.Json.lookup j "efaInfo") EfaInfo.of_json
  ; encryption_in_transit_supported =
      Aws.Util.option_map
        (Aws.Json.lookup j "encryptionInTransitSupported")
        Boolean.of_json
  ; ena_srd_supported =
      Aws.Util.option_map (Aws.Json.lookup j "enaSrdSupported") Boolean.of_json
  ; bandwidth_weightings =
      BandwidthWeightingTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "bandwidthWeightings"))
  ; flexible_ena_queues_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "flexibleEnaQueuesSupport")
        FlexibleEnaQueuesSupport.of_json
  ; connection_tracking_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionTrackingConfiguration")
        DefaultConnectionTrackingConfiguration.of_json
  ; secondary_network_supported =
      Aws.Util.option_map (Aws.Json.lookup j "secondaryNetworkSupported") Boolean.of_json
  ; maximum_secondary_network_interfaces =
      Aws.Util.option_map
        (Aws.Json.lookup j "maximumSecondaryNetworkInterfaces")
        Integer.of_json
  ; ipv4_addresses_per_secondary_interface =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipv4AddressesPerSecondaryInterface")
        Integer.of_json
  }
