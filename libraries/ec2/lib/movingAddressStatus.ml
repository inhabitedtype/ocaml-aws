open Aws.BaseTypes

type t =
  { move_status : MoveStatus.t option
  ; public_ip : String.t option
  }

let make ?move_status ?public_ip () = { move_status; public_ip }

let parse xml =
  Some
    { move_status =
        Aws.Util.option_bind (Aws.Xml.member "moveStatus" xml) MoveStatus.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.move_status (fun f ->
             Aws.Query.Pair ("MoveStatus", MoveStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ; Aws.Util.option_map v.move_status (fun f -> "moveStatus", MoveStatus.to_json f)
       ])

let of_json j =
  { move_status = Aws.Util.option_map (Aws.Json.lookup j "moveStatus") MoveStatus.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  }
