open Aws.BaseTypes

type t =
  { name : OperationResultFilterName.t option
  ; values : String.t option
  }

let make ?name ?values () = { name; values }

let parse xml =
  Some
    { name =
        Aws.Util.option_bind (Aws.Xml.member "Name" xml) OperationResultFilterName.parse
    ; values = Aws.Util.option_bind (Aws.Xml.member "Values" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.values (fun f ->
             Aws.Query.Pair ("Values", String.to_query f))
       ; Aws.Util.option_map v.name (fun f ->
             Aws.Query.Pair ("Name", OperationResultFilterName.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.values (fun f -> "Values", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", OperationResultFilterName.to_json f)
       ])

let of_json j =
  { name =
      Aws.Util.option_map (Aws.Json.lookup j "Name") OperationResultFilterName.of_json
  ; values = Aws.Util.option_map (Aws.Json.lookup j "Values") String.of_json
  }
