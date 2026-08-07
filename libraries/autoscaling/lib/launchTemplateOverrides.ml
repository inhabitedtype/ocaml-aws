open Aws.BaseTypes

type t =
  { instance_type : String.t option
  ; weighted_capacity : String.t option
  ; launch_template_specification : LaunchTemplateSpecification.t option
  ; instance_requirements : InstanceRequirements.t option
  ; image_id : String.t option
  }

let make
    ?instance_type
    ?weighted_capacity
    ?launch_template_specification
    ?instance_requirements
    ?image_id
    () =
  { instance_type
  ; weighted_capacity
  ; launch_template_specification
  ; instance_requirements
  ; image_id
  }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse
    ; weighted_capacity =
        Aws.Util.option_bind (Aws.Xml.member "WeightedCapacity" xml) String.parse
    ; launch_template_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplateSpecification" xml)
          LaunchTemplateSpecification.parse
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceRequirements" xml)
          InstanceRequirements.parse
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair ("InstanceRequirements", InstanceRequirements.to_query f))
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             Aws.Query.Pair
               ("LaunchTemplateSpecification", LaunchTemplateSpecification.to_query f))
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             Aws.Query.Pair ("WeightedCapacity", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             "InstanceRequirements", InstanceRequirements.to_json f)
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             "LaunchTemplateSpecification", LaunchTemplateSpecification.to_json f)
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             "WeightedCapacity", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "InstanceType", String.to_json f)
       ])

let of_json j =
  { instance_type = Aws.Util.option_map (Aws.Json.lookup j "InstanceType") String.of_json
  ; weighted_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "WeightedCapacity") String.of_json
  ; launch_template_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplateSpecification")
        LaunchTemplateSpecification.of_json
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceRequirements")
        InstanceRequirements.of_json
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
  }
