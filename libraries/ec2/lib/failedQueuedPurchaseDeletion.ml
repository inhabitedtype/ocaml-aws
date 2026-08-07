open Aws.BaseTypes

type t =
  { error : DeleteQueuedReservedInstancesError.t option
  ; reserved_instances_id : String.t option
  }

let make ?error ?reserved_instances_id () = { error; reserved_instances_id }

let parse xml =
  Some
    { error =
        Aws.Util.option_bind
          (Aws.Xml.member "error" xml)
          DeleteQueuedReservedInstancesError.parse
    ; reserved_instances_id =
        Aws.Util.option_bind (Aws.Xml.member "reservedInstancesId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instances_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesId", String.to_query f))
       ; Aws.Util.option_map v.error (fun f ->
             Aws.Query.Pair ("Error", DeleteQueuedReservedInstancesError.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instances_id (fun f ->
             "reservedInstancesId", String.to_json f)
       ; Aws.Util.option_map v.error (fun f ->
             "error", DeleteQueuedReservedInstancesError.to_json f)
       ])

let of_json j =
  { error =
      Aws.Util.option_map
        (Aws.Json.lookup j "error")
        DeleteQueuedReservedInstancesError.of_json
  ; reserved_instances_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesId") String.of_json
  }
