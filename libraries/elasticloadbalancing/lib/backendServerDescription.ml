open Aws.BaseTypes

type t =
  { instance_port : Integer.t option
  ; policy_names : PolicyNames.t
  }

let make ?instance_port ?(policy_names = []) () = { instance_port; policy_names }

let parse xml =
  Some
    { instance_port =
        Aws.Util.option_bind (Aws.Xml.member "InstancePort" xml) Integer.parse
    ; policy_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
       ; Aws.Util.option_map v.instance_port (fun f ->
             Aws.Query.Pair ("InstancePort", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
       ; Aws.Util.option_map v.instance_port (fun f -> "InstancePort", Integer.to_json f)
       ])

let of_json j =
  { instance_port = Aws.Util.option_map (Aws.Json.lookup j "InstancePort") Integer.of_json
  ; policy_names =
      PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
  }
