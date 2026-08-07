open Aws.BaseTypes

type t =
  { baseline_bandwidth_in_mbps : Integer.t option
  ; baseline_throughput_in_m_bps : Double.t option
  ; baseline_iops : Integer.t option
  ; maximum_bandwidth_in_mbps : Integer.t option
  ; maximum_throughput_in_m_bps : Double.t option
  ; maximum_iops : Integer.t option
  }

let make
    ?baseline_bandwidth_in_mbps
    ?baseline_throughput_in_m_bps
    ?baseline_iops
    ?maximum_bandwidth_in_mbps
    ?maximum_throughput_in_m_bps
    ?maximum_iops
    () =
  { baseline_bandwidth_in_mbps
  ; baseline_throughput_in_m_bps
  ; baseline_iops
  ; maximum_bandwidth_in_mbps
  ; maximum_throughput_in_m_bps
  ; maximum_iops
  }

let parse xml =
  Some
    { baseline_bandwidth_in_mbps =
        Aws.Util.option_bind (Aws.Xml.member "baselineBandwidthInMbps" xml) Integer.parse
    ; baseline_throughput_in_m_bps =
        Aws.Util.option_bind (Aws.Xml.member "baselineThroughputInMBps" xml) Double.parse
    ; baseline_iops =
        Aws.Util.option_bind (Aws.Xml.member "baselineIops" xml) Integer.parse
    ; maximum_bandwidth_in_mbps =
        Aws.Util.option_bind (Aws.Xml.member "maximumBandwidthInMbps" xml) Integer.parse
    ; maximum_throughput_in_m_bps =
        Aws.Util.option_bind (Aws.Xml.member "maximumThroughputInMBps" xml) Double.parse
    ; maximum_iops = Aws.Util.option_bind (Aws.Xml.member "maximumIops" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_iops (fun f ->
             Aws.Query.Pair ("MaximumIops", Integer.to_query f))
       ; Aws.Util.option_map v.maximum_throughput_in_m_bps (fun f ->
             Aws.Query.Pair ("MaximumThroughputInMBps", Double.to_query f))
       ; Aws.Util.option_map v.maximum_bandwidth_in_mbps (fun f ->
             Aws.Query.Pair ("MaximumBandwidthInMbps", Integer.to_query f))
       ; Aws.Util.option_map v.baseline_iops (fun f ->
             Aws.Query.Pair ("BaselineIops", Integer.to_query f))
       ; Aws.Util.option_map v.baseline_throughput_in_m_bps (fun f ->
             Aws.Query.Pair ("BaselineThroughputInMBps", Double.to_query f))
       ; Aws.Util.option_map v.baseline_bandwidth_in_mbps (fun f ->
             Aws.Query.Pair ("BaselineBandwidthInMbps", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_iops (fun f -> "maximumIops", Integer.to_json f)
       ; Aws.Util.option_map v.maximum_throughput_in_m_bps (fun f ->
             "maximumThroughputInMBps", Double.to_json f)
       ; Aws.Util.option_map v.maximum_bandwidth_in_mbps (fun f ->
             "maximumBandwidthInMbps", Integer.to_json f)
       ; Aws.Util.option_map v.baseline_iops (fun f -> "baselineIops", Integer.to_json f)
       ; Aws.Util.option_map v.baseline_throughput_in_m_bps (fun f ->
             "baselineThroughputInMBps", Double.to_json f)
       ; Aws.Util.option_map v.baseline_bandwidth_in_mbps (fun f ->
             "baselineBandwidthInMbps", Integer.to_json f)
       ])

let of_json j =
  { baseline_bandwidth_in_mbps =
      Aws.Util.option_map (Aws.Json.lookup j "baselineBandwidthInMbps") Integer.of_json
  ; baseline_throughput_in_m_bps =
      Aws.Util.option_map (Aws.Json.lookup j "baselineThroughputInMBps") Double.of_json
  ; baseline_iops = Aws.Util.option_map (Aws.Json.lookup j "baselineIops") Integer.of_json
  ; maximum_bandwidth_in_mbps =
      Aws.Util.option_map (Aws.Json.lookup j "maximumBandwidthInMbps") Integer.of_json
  ; maximum_throughput_in_m_bps =
      Aws.Util.option_map (Aws.Json.lookup j "maximumThroughputInMBps") Double.of_json
  ; maximum_iops = Aws.Util.option_map (Aws.Json.lookup j "maximumIops") Integer.of_json
  }
