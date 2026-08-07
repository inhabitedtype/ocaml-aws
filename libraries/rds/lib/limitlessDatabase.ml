open Aws.BaseTypes

type t =
  { status : LimitlessDatabaseStatus.t option
  ; min_required_a_c_u : Double.t option
  }

let make ?status ?min_required_a_c_u () = { status; min_required_a_c_u }

let parse xml =
  Some
    { status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) LimitlessDatabaseStatus.parse
    ; min_required_a_c_u =
        Aws.Util.option_bind (Aws.Xml.member "MinRequiredACU" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.min_required_a_c_u (fun f ->
             Aws.Query.Pair ("MinRequiredACU", Double.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", LimitlessDatabaseStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.min_required_a_c_u (fun f ->
             "MinRequiredACU", Double.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "Status", LimitlessDatabaseStatus.to_json f)
       ])

let of_json j =
  { status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") LimitlessDatabaseStatus.of_json
  ; min_required_a_c_u =
      Aws.Util.option_map (Aws.Json.lookup j "MinRequiredACU") Double.of_json
  }
