open Aws.BaseTypes

type t =
  { public_ip : String.t option
  ; status : Status.t option
  }

let make ?public_ip ?status () = { public_ip; status }

let parse xml =
  Some
    { public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) Status.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", Status.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", Status.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ])

let of_json j =
  { public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") Status.of_json
  }
