open Aws.BaseTypes

type t =
  { alarm_contributors : AlarmContributors.t
  ; next_token : String.t option
  }

let make ~alarm_contributors ?next_token () = { alarm_contributors; next_token }

let parse xml =
  Some
    { alarm_contributors =
        Aws.Xml.required
          "AlarmContributors"
          (Aws.Util.option_bind
             (Aws.Xml.member "AlarmContributors" xml)
             AlarmContributors.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AlarmContributors.member", AlarmContributors.to_query v.alarm_contributors))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("AlarmContributors", AlarmContributors.to_json v.alarm_contributors)
       ])

let of_json j =
  { alarm_contributors =
      AlarmContributors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmContributors"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
