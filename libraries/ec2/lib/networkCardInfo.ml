open Aws.BaseTypes

type t =
  { network_card_index : Integer.t option
  ; network_performance : String.t option
  ; maximum_network_interfaces : Integer.t option
  ; additional_flexible_network_interfaces : Integer.t option
  ; baseline_bandwidth_in_gbps : Double.t option
  ; peak_bandwidth_in_gbps : Double.t option
  ; default_ena_queue_count_per_interface : Integer.t option
  ; maximum_ena_queue_count : Integer.t option
  ; maximum_ena_queue_count_per_interface : Integer.t option
  }

let make
    ?network_card_index
    ?network_performance
    ?maximum_network_interfaces
    ?additional_flexible_network_interfaces
    ?baseline_bandwidth_in_gbps
    ?peak_bandwidth_in_gbps
    ?default_ena_queue_count_per_interface
    ?maximum_ena_queue_count
    ?maximum_ena_queue_count_per_interface
    () =
  { network_card_index
  ; network_performance
  ; maximum_network_interfaces
  ; additional_flexible_network_interfaces
  ; baseline_bandwidth_in_gbps
  ; peak_bandwidth_in_gbps
  ; default_ena_queue_count_per_interface
  ; maximum_ena_queue_count
  ; maximum_ena_queue_count_per_interface
  }

let parse xml =
  Some
    { network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "networkCardIndex" xml) Integer.parse
    ; network_performance =
        Aws.Util.option_bind (Aws.Xml.member "networkPerformance" xml) String.parse
    ; maximum_network_interfaces =
        Aws.Util.option_bind (Aws.Xml.member "maximumNetworkInterfaces" xml) Integer.parse
    ; additional_flexible_network_interfaces =
        Aws.Util.option_bind
          (Aws.Xml.member "additionalFlexibleNetworkInterfaces" xml)
          Integer.parse
    ; baseline_bandwidth_in_gbps =
        Aws.Util.option_bind (Aws.Xml.member "baselineBandwidthInGbps" xml) Double.parse
    ; peak_bandwidth_in_gbps =
        Aws.Util.option_bind (Aws.Xml.member "peakBandwidthInGbps" xml) Double.parse
    ; default_ena_queue_count_per_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultEnaQueueCountPerInterface" xml)
          Integer.parse
    ; maximum_ena_queue_count =
        Aws.Util.option_bind (Aws.Xml.member "maximumEnaQueueCount" xml) Integer.parse
    ; maximum_ena_queue_count_per_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "maximumEnaQueueCountPerInterface" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_ena_queue_count_per_interface (fun f ->
             Aws.Query.Pair ("MaximumEnaQueueCountPerInterface", Integer.to_query f))
       ; Aws.Util.option_map v.maximum_ena_queue_count (fun f ->
             Aws.Query.Pair ("MaximumEnaQueueCount", Integer.to_query f))
       ; Aws.Util.option_map v.default_ena_queue_count_per_interface (fun f ->
             Aws.Query.Pair ("DefaultEnaQueueCountPerInterface", Integer.to_query f))
       ; Aws.Util.option_map v.peak_bandwidth_in_gbps (fun f ->
             Aws.Query.Pair ("PeakBandwidthInGbps", Double.to_query f))
       ; Aws.Util.option_map v.baseline_bandwidth_in_gbps (fun f ->
             Aws.Query.Pair ("BaselineBandwidthInGbps", Double.to_query f))
       ; Aws.Util.option_map v.additional_flexible_network_interfaces (fun f ->
             Aws.Query.Pair ("AdditionalFlexibleNetworkInterfaces", Integer.to_query f))
       ; Aws.Util.option_map v.maximum_network_interfaces (fun f ->
             Aws.Query.Pair ("MaximumNetworkInterfaces", Integer.to_query f))
       ; Aws.Util.option_map v.network_performance (fun f ->
             Aws.Query.Pair ("NetworkPerformance", String.to_query f))
       ; Aws.Util.option_map v.network_card_index (fun f ->
             Aws.Query.Pair ("NetworkCardIndex", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_ena_queue_count_per_interface (fun f ->
             "maximumEnaQueueCountPerInterface", Integer.to_json f)
       ; Aws.Util.option_map v.maximum_ena_queue_count (fun f ->
             "maximumEnaQueueCount", Integer.to_json f)
       ; Aws.Util.option_map v.default_ena_queue_count_per_interface (fun f ->
             "defaultEnaQueueCountPerInterface", Integer.to_json f)
       ; Aws.Util.option_map v.peak_bandwidth_in_gbps (fun f ->
             "peakBandwidthInGbps", Double.to_json f)
       ; Aws.Util.option_map v.baseline_bandwidth_in_gbps (fun f ->
             "baselineBandwidthInGbps", Double.to_json f)
       ; Aws.Util.option_map v.additional_flexible_network_interfaces (fun f ->
             "additionalFlexibleNetworkInterfaces", Integer.to_json f)
       ; Aws.Util.option_map v.maximum_network_interfaces (fun f ->
             "maximumNetworkInterfaces", Integer.to_json f)
       ; Aws.Util.option_map v.network_performance (fun f ->
             "networkPerformance", String.to_json f)
       ; Aws.Util.option_map v.network_card_index (fun f ->
             "networkCardIndex", Integer.to_json f)
       ])

let of_json j =
  { network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "networkCardIndex") Integer.of_json
  ; network_performance =
      Aws.Util.option_map (Aws.Json.lookup j "networkPerformance") String.of_json
  ; maximum_network_interfaces =
      Aws.Util.option_map (Aws.Json.lookup j "maximumNetworkInterfaces") Integer.of_json
  ; additional_flexible_network_interfaces =
      Aws.Util.option_map
        (Aws.Json.lookup j "additionalFlexibleNetworkInterfaces")
        Integer.of_json
  ; baseline_bandwidth_in_gbps =
      Aws.Util.option_map (Aws.Json.lookup j "baselineBandwidthInGbps") Double.of_json
  ; peak_bandwidth_in_gbps =
      Aws.Util.option_map (Aws.Json.lookup j "peakBandwidthInGbps") Double.of_json
  ; default_ena_queue_count_per_interface =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultEnaQueueCountPerInterface")
        Integer.of_json
  ; maximum_ena_queue_count =
      Aws.Util.option_map (Aws.Json.lookup j "maximumEnaQueueCount") Integer.of_json
  ; maximum_ena_queue_count_per_interface =
      Aws.Util.option_map
        (Aws.Json.lookup j "maximumEnaQueueCountPerInterface")
        Integer.of_json
  }
