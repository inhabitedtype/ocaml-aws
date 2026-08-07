open Aws.BaseTypes

type t = { reserved_instances_modification_id : String.t option }

let make ?reserved_instances_modification_id () = { reserved_instances_modification_id }

let parse xml =
  Some
    { reserved_instances_modification_id =
        Aws.Util.option_bind
          (Aws.Xml.member "reservedInstancesModificationId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instances_modification_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesModificationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instances_modification_id (fun f ->
             "reservedInstancesModificationId", String.to_json f)
       ])

let of_json j =
  { reserved_instances_modification_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservedInstancesModificationId")
        String.of_json
  }
