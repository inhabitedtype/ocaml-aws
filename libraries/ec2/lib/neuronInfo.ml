open Aws.BaseTypes

type t =
  { neuron_devices : NeuronDeviceInfoList.t
  ; total_neuron_device_memory_in_mi_b : Integer.t option
  }

let make ?(neuron_devices = []) ?total_neuron_device_memory_in_mi_b () =
  { neuron_devices; total_neuron_device_memory_in_mi_b }

let parse xml =
  Some
    { neuron_devices =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "neuronDevices" xml)
             NeuronDeviceInfoList.parse)
    ; total_neuron_device_memory_in_mi_b =
        Aws.Util.option_bind
          (Aws.Xml.member "totalNeuronDeviceMemoryInMiB" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_neuron_device_memory_in_mi_b (fun f ->
             Aws.Query.Pair ("TotalNeuronDeviceMemoryInMiB", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("NeuronDevices", NeuronDeviceInfoList.to_query v.neuron_devices))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_neuron_device_memory_in_mi_b (fun f ->
             "totalNeuronDeviceMemoryInMiB", Integer.to_json f)
       ; Some ("neuronDevices", NeuronDeviceInfoList.to_json v.neuron_devices)
       ])

let of_json j =
  { neuron_devices =
      NeuronDeviceInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "neuronDevices"))
  ; total_neuron_device_memory_in_mi_b =
      Aws.Util.option_map
        (Aws.Json.lookup j "totalNeuronDeviceMemoryInMiB")
        Integer.of_json
  }
