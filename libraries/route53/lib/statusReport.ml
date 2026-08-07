open Aws.BaseTypes

type t =
  { status : String.t option
  ; checked_time : DateTime.t option
  }

let make ?status ?checked_time () = { status; checked_time }

let parse xml =
  Some
    { status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; checked_time =
        Aws.Util.option_bind (Aws.Xml.member "CheckedTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.checked_time (fun f ->
             Aws.Query.Pair ("CheckedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.checked_time (fun f -> "CheckedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ])

let of_json j =
  { status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; checked_time = Aws.Util.option_map (Aws.Json.lookup j "CheckedTime") DateTime.of_json
  }
