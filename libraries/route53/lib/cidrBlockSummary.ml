open Aws.BaseTypes

type t =
  { cidr_block : String.t option
  ; location_name : String.t option
  }

let make ?cidr_block ?location_name () = { cidr_block; location_name }

let parse xml =
  Some
    { cidr_block = Aws.Util.option_bind (Aws.Xml.member "CidrBlock" xml) String.parse
    ; location_name =
        Aws.Util.option_bind (Aws.Xml.member "LocationName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location_name (fun f ->
             Aws.Query.Pair ("LocationName", String.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location_name (fun f -> "LocationName", String.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "CidrBlock", String.to_json f)
       ])

let of_json j =
  { cidr_block = Aws.Util.option_map (Aws.Json.lookup j "CidrBlock") String.of_json
  ; location_name = Aws.Util.option_map (Aws.Json.lookup j "LocationName") String.of_json
  }
