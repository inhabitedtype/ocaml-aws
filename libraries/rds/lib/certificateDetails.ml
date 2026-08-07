open Aws.BaseTypes

type t =
  { c_a_identifier : String.t option
  ; valid_till : DateTime.t option
  }

let make ?c_a_identifier ?valid_till () = { c_a_identifier; valid_till }

let parse xml =
  Some
    { c_a_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CAIdentifier" xml) String.parse
    ; valid_till = Aws.Util.option_bind (Aws.Xml.member "ValidTill" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.valid_till (fun f ->
             Aws.Query.Pair ("ValidTill", DateTime.to_query f))
       ; Aws.Util.option_map v.c_a_identifier (fun f ->
             Aws.Query.Pair ("CAIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.valid_till (fun f -> "ValidTill", DateTime.to_json f)
       ; Aws.Util.option_map v.c_a_identifier (fun f -> "CAIdentifier", String.to_json f)
       ])

let of_json j =
  { c_a_identifier = Aws.Util.option_map (Aws.Json.lookup j "CAIdentifier") String.of_json
  ; valid_till = Aws.Util.option_map (Aws.Json.lookup j "ValidTill") DateTime.of_json
  }
