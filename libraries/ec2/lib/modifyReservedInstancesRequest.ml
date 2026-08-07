open Aws.BaseTypes

type t =
  { reserved_instances_ids : ReservedInstancesIdStringList.t
  ; client_token : String.t option
  ; target_configurations : ReservedInstancesConfigurationList.t
  }

let make ~reserved_instances_ids ?client_token ~target_configurations () =
  { reserved_instances_ids; client_token; target_configurations }

let parse xml =
  Some
    { reserved_instances_ids =
        Aws.Xml.required
          "ReservedInstancesId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstancesId" xml)
             ReservedInstancesIdStringList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; target_configurations =
        Aws.Xml.required
          "ReservedInstancesConfigurationSetItemType"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstancesConfigurationSetItemType" xml)
             ReservedInstancesConfigurationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ReservedInstancesConfigurationSetItemType"
              , ReservedInstancesConfigurationList.to_query v.target_configurations ))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstancesId"
              , ReservedInstancesIdStringList.to_query v.reserved_instances_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ReservedInstancesConfigurationSetItemType"
           , ReservedInstancesConfigurationList.to_json v.target_configurations )
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Some
           ( "ReservedInstancesId"
           , ReservedInstancesIdStringList.to_json v.reserved_instances_ids )
       ])

let of_json j =
  { reserved_instances_ids =
      ReservedInstancesIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedInstancesId"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; target_configurations =
      ReservedInstancesConfigurationList.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "ReservedInstancesConfigurationSetItemType"))
  }
