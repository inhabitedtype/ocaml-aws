open Aws.BaseTypes

type t =
  { queue_name : String.t
  ; queue_owner_a_w_s_account_id : String.t option
  }

let make ~queue_name ?queue_owner_a_w_s_account_id () =
  { queue_name; queue_owner_a_w_s_account_id }

let parse xml =
  Some
    { queue_name =
        Aws.Xml.required
          "QueueName"
          (Aws.Util.option_bind (Aws.Xml.member "QueueName" xml) String.parse)
    ; queue_owner_a_w_s_account_id =
        Aws.Util.option_bind (Aws.Xml.member "QueueOwnerAWSAccountId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.queue_owner_a_w_s_account_id (fun f ->
             Aws.Query.Pair ("QueueOwnerAWSAccountId", String.to_query f))
       ; Some (Aws.Query.Pair ("QueueName", String.to_query v.queue_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.queue_owner_a_w_s_account_id (fun f ->
             "QueueOwnerAWSAccountId", String.to_json f)
       ; Some ("QueueName", String.to_json v.queue_name)
       ])

let of_json j =
  { queue_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueName"))
  ; queue_owner_a_w_s_account_id =
      Aws.Util.option_map (Aws.Json.lookup j "QueueOwnerAWSAccountId") String.of_json
  }
