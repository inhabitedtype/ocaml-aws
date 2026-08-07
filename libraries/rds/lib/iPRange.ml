open Aws.BaseTypes

type t =
  { status : String.t option
  ; c_i_d_r_i_p : String.t option
  }

let make ?status ?c_i_d_r_i_p () = { status; c_i_d_r_i_p }

let parse xml =
  Some
    { status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; c_i_d_r_i_p = Aws.Util.option_bind (Aws.Xml.member "CIDRIP" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.c_i_d_r_i_p (fun f ->
             Aws.Query.Pair ("CIDRIP", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.c_i_d_r_i_p (fun f -> "CIDRIP", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ])

let of_json j =
  { status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; c_i_d_r_i_p = Aws.Util.option_map (Aws.Json.lookup j "CIDRIP") String.of_json
  }
