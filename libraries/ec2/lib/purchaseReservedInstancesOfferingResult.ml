open Aws.BaseTypes

type t = { reserved_instances_id : String.t option }

let make ?reserved_instances_id () = { reserved_instances_id }

let parse xml =
  Some
    { reserved_instances_id =
        Aws.Util.option_bind (Aws.Xml.member "reservedInstancesId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instances_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instances_id (fun f ->
             "reservedInstancesId", String.to_json f)
       ])

let of_json j =
  { reserved_instances_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesId") String.of_json
  }
