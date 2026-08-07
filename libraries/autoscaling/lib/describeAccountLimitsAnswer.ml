open Aws.BaseTypes

type t =
  { max_number_of_auto_scaling_groups : Integer.t option
  ; max_number_of_launch_configurations : Integer.t option
  ; number_of_auto_scaling_groups : Integer.t option
  ; number_of_launch_configurations : Integer.t option
  }

let make
    ?max_number_of_auto_scaling_groups
    ?max_number_of_launch_configurations
    ?number_of_auto_scaling_groups
    ?number_of_launch_configurations
    () =
  { max_number_of_auto_scaling_groups
  ; max_number_of_launch_configurations
  ; number_of_auto_scaling_groups
  ; number_of_launch_configurations
  }

let parse xml =
  Some
    { max_number_of_auto_scaling_groups =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxNumberOfAutoScalingGroups" xml)
          Integer.parse
    ; max_number_of_launch_configurations =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxNumberOfLaunchConfigurations" xml)
          Integer.parse
    ; number_of_auto_scaling_groups =
        Aws.Util.option_bind
          (Aws.Xml.member "NumberOfAutoScalingGroups" xml)
          Integer.parse
    ; number_of_launch_configurations =
        Aws.Util.option_bind
          (Aws.Xml.member "NumberOfLaunchConfigurations" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_launch_configurations (fun f ->
             Aws.Query.Pair ("NumberOfLaunchConfigurations", Integer.to_query f))
       ; Aws.Util.option_map v.number_of_auto_scaling_groups (fun f ->
             Aws.Query.Pair ("NumberOfAutoScalingGroups", Integer.to_query f))
       ; Aws.Util.option_map v.max_number_of_launch_configurations (fun f ->
             Aws.Query.Pair ("MaxNumberOfLaunchConfigurations", Integer.to_query f))
       ; Aws.Util.option_map v.max_number_of_auto_scaling_groups (fun f ->
             Aws.Query.Pair ("MaxNumberOfAutoScalingGroups", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_launch_configurations (fun f ->
             "NumberOfLaunchConfigurations", Integer.to_json f)
       ; Aws.Util.option_map v.number_of_auto_scaling_groups (fun f ->
             "NumberOfAutoScalingGroups", Integer.to_json f)
       ; Aws.Util.option_map v.max_number_of_launch_configurations (fun f ->
             "MaxNumberOfLaunchConfigurations", Integer.to_json f)
       ; Aws.Util.option_map v.max_number_of_auto_scaling_groups (fun f ->
             "MaxNumberOfAutoScalingGroups", Integer.to_json f)
       ])

let of_json j =
  { max_number_of_auto_scaling_groups =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaxNumberOfAutoScalingGroups")
        Integer.of_json
  ; max_number_of_launch_configurations =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaxNumberOfLaunchConfigurations")
        Integer.of_json
  ; number_of_auto_scaling_groups =
      Aws.Util.option_map (Aws.Json.lookup j "NumberOfAutoScalingGroups") Integer.of_json
  ; number_of_launch_configurations =
      Aws.Util.option_map
        (Aws.Json.lookup j "NumberOfLaunchConfigurations")
        Integer.of_json
  }
