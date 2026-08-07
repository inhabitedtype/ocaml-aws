type t =
  { architecture_types : ArchitectureTypeSet.t
  ; virtualization_types : VirtualizationTypeSet.t
  ; instance_requirements : InstanceRequirementsRequest.t option
  }

let make ?(architecture_types = []) ?(virtualization_types = []) ?instance_requirements ()
    =
  { architecture_types; virtualization_types; instance_requirements }

let parse xml =
  Some
    { architecture_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ArchitectureType" xml)
             ArchitectureTypeSet.parse)
    ; virtualization_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VirtualizationType" xml)
             VirtualizationTypeSet.parse)
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceRequirements" xml)
          InstanceRequirementsRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair
               ("InstanceRequirements", InstanceRequirementsRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VirtualizationType", VirtualizationTypeSet.to_query v.virtualization_types))
       ; Some
           (Aws.Query.Pair
              ("ArchitectureType", ArchitectureTypeSet.to_query v.architecture_types))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_requirements (fun f ->
             "InstanceRequirements", InstanceRequirementsRequest.to_json f)
       ; Some ("VirtualizationType", VirtualizationTypeSet.to_json v.virtualization_types)
       ; Some ("ArchitectureType", ArchitectureTypeSet.to_json v.architecture_types)
       ])

let of_json j =
  { architecture_types =
      ArchitectureTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ArchitectureType"))
  ; virtualization_types =
      VirtualizationTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VirtualizationType"))
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceRequirements")
        InstanceRequirementsRequest.of_json
  }
