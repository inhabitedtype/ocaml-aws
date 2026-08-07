open Aws.BaseTypes

type t =
  { reserved_instances_id : String.t option
  ; target_configuration : ReservedInstancesConfiguration.t option
  }

let make ?reserved_instances_id ?target_configuration () =
  { reserved_instances_id; target_configuration }

let parse xml =
  Some
    { reserved_instances_id =
        Aws.Util.option_bind (Aws.Xml.member "reservedInstancesId" xml) String.parse
    ; target_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "targetConfiguration" xml)
          ReservedInstancesConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_configuration (fun f ->
             Aws.Query.Pair
               ("TargetConfiguration", ReservedInstancesConfiguration.to_query f))
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_configuration (fun f ->
             "targetConfiguration", ReservedInstancesConfiguration.to_json f)
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             "reservedInstancesId", String.to_json f)
       ])

let of_json j =
  { reserved_instances_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesId") String.of_json
  ; target_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetConfiguration")
        ReservedInstancesConfiguration.of_json
  }
