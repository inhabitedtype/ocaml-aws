open Aws.BaseTypes

type t =
  { vpc_security_group_id : String.t option
  ; status : String.t option
  }

let make ?vpc_security_group_id ?status () = { vpc_security_group_id; status }

let parse xml =
  Some
    { vpc_security_group_id =
        Aws.Util.option_bind (Aws.Xml.member "VpcSecurityGroupId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.vpc_security_group_id (fun f ->
             Aws.Query.Pair ("VpcSecurityGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.vpc_security_group_id (fun f ->
             "VpcSecurityGroupId", String.to_json f)
       ])

let of_json j =
  { vpc_security_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "VpcSecurityGroupId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
