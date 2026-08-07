open Aws.BaseTypes

type t =
  { d_b_security_group_name : String.t option
  ; status : String.t option
  }

let make ?d_b_security_group_name ?status () = { d_b_security_group_name; status }

let parse xml =
  Some
    { d_b_security_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSecurityGroupName" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.d_b_security_group_name (fun f ->
             Aws.Query.Pair ("DBSecurityGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.d_b_security_group_name (fun f ->
             "DBSecurityGroupName", String.to_json f)
       ])

let of_json j =
  { d_b_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSecurityGroupName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
