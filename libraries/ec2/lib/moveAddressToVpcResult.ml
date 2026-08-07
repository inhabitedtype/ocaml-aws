open Aws.BaseTypes

type t =
  { allocation_id : String.t option
  ; status : Status.t option
  }

let make ?allocation_id ?status () = { allocation_id; status }

let parse xml =
  Some
    { allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "allocationId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) Status.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", Status.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", Status.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "allocationId", String.to_json f)
       ])

let of_json j =
  { allocation_id = Aws.Util.option_map (Aws.Json.lookup j "allocationId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") Status.of_json
  }
