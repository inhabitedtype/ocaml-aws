open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; stack_status_filter : StackStatusFilter.t
  }

let make ?next_token ?(stack_status_filter = []) () = { next_token; stack_status_filter }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; stack_status_filter =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StackStatusFilter" xml)
             StackStatusFilter.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "StackStatusFilter.member"
              , StackStatusFilter.to_query v.stack_status_filter ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StackStatusFilter", StackStatusFilter.to_json v.stack_status_filter)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; stack_status_filter =
      StackStatusFilter.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackStatusFilter"))
  }
