open Aws.BaseTypes

type t =
  { subnet_id : String.t option
  ; ipv4 : String.t option
  ; ipv6 : String.t option
  }

let make ?subnet_id ?ipv4 ?ipv6 () = { subnet_id; ipv4; ipv6 }

let parse xml =
  Some
    { subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    ; ipv4 = Aws.Util.option_bind (Aws.Xml.member "Ipv4" xml) String.parse
    ; ipv6 = Aws.Util.option_bind (Aws.Xml.member "Ipv6" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6 (fun f -> Aws.Query.Pair ("Ipv6", String.to_query f))
       ; Aws.Util.option_map v.ipv4 (fun f -> Aws.Query.Pair ("Ipv4", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6 (fun f -> "Ipv6", String.to_json f)
       ; Aws.Util.option_map v.ipv4 (fun f -> "Ipv4", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ])

let of_json j =
  { subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  ; ipv4 = Aws.Util.option_map (Aws.Json.lookup j "Ipv4") String.of_json
  ; ipv6 = Aws.Util.option_map (Aws.Json.lookup j "Ipv6") String.of_json
  }
