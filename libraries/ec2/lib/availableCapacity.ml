open Aws.BaseTypes

type t =
  { available_instance_capacity : AvailableInstanceCapacityList.t
  ; available_v_cpus : Integer.t option
  }

let make ?(available_instance_capacity = []) ?available_v_cpus () =
  { available_instance_capacity; available_v_cpus }

let parse xml =
  Some
    { available_instance_capacity =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availableInstanceCapacity" xml)
             AvailableInstanceCapacityList.parse)
    ; available_v_cpus =
        Aws.Util.option_bind (Aws.Xml.member "availableVCpus" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.available_v_cpus (fun f ->
             Aws.Query.Pair ("AvailableVCpus", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AvailableInstanceCapacity"
              , AvailableInstanceCapacityList.to_query v.available_instance_capacity ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.available_v_cpus (fun f ->
             "availableVCpus", Integer.to_json f)
       ; Some
           ( "availableInstanceCapacity"
           , AvailableInstanceCapacityList.to_json v.available_instance_capacity )
       ])

let of_json j =
  { available_instance_capacity =
      AvailableInstanceCapacityList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availableInstanceCapacity"))
  ; available_v_cpus =
      Aws.Util.option_map (Aws.Json.lookup j "availableVCpus") Integer.of_json
  }
