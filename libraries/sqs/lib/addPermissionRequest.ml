open Aws.BaseTypes

type t =
  { queue_url : String.t
  ; label : String.t
  ; a_w_s_account_ids : AWSAccountIdList.t
  ; actions : ActionNameList.t
  }

let make ~queue_url ~label ~a_w_s_account_ids ~actions () =
  { queue_url; label; a_w_s_account_ids; actions }

let parse xml =
  Some
    { queue_url =
        Aws.Xml.required
          "QueueUrl"
          (Aws.Util.option_bind (Aws.Xml.member "QueueUrl" xml) String.parse)
    ; label =
        Aws.Xml.required
          "Label"
          (Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse)
    ; a_w_s_account_ids = Aws.Xml.required "AWSAccountIds" (AWSAccountIdList.parse xml)
    ; actions = Aws.Xml.required "Actions" (ActionNameList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Actions.member", ActionNameList.to_query v.actions))
       ; Some
           (Aws.Query.Pair
              ("AWSAccountIds.member", AWSAccountIdList.to_query v.a_w_s_account_ids))
       ; Some (Aws.Query.Pair ("Label", String.to_query v.label))
       ; Some (Aws.Query.Pair ("QueueUrl", String.to_query v.queue_url))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Actions", ActionNameList.to_json v.actions)
       ; Some ("AWSAccountIds", AWSAccountIdList.to_json v.a_w_s_account_ids)
       ; Some ("Label", String.to_json v.label)
       ; Some ("QueueUrl", String.to_json v.queue_url)
       ])

let of_json j =
  { queue_url = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueUrl"))
  ; label = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Label"))
  ; a_w_s_account_ids =
      AWSAccountIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AWSAccountIds"))
  ; actions =
      ActionNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Actions"))
  }
