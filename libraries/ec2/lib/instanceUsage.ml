open Aws.BaseTypes

type t =
  { account_id : String.t option
  ; used_instance_count : Integer.t option
  }

let make ?account_id ?used_instance_count () = { account_id; used_instance_count }

let parse xml =
  Some
    { account_id = Aws.Util.option_bind (Aws.Xml.member "accountId" xml) String.parse
    ; used_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "usedInstanceCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.used_instance_count (fun f ->
             Aws.Query.Pair ("UsedInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.account_id (fun f ->
             Aws.Query.Pair ("AccountId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.used_instance_count (fun f ->
             "usedInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.account_id (fun f -> "accountId", String.to_json f)
       ])

let of_json j =
  { account_id = Aws.Util.option_map (Aws.Json.lookup j "accountId") String.of_json
  ; used_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "usedInstanceCount") Integer.of_json
  }
