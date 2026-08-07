open Aws.BaseTypes

type t =
  { service_names : ValueStringList.t
  ; service_details : ServiceDetailSet.t
  ; next_token : String.t option
  }

let make ?(service_names = []) ?(service_details = []) ?next_token () =
  { service_names; service_details; next_token }

let parse xml =
  Some
    { service_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "serviceNameSet" xml)
             ValueStringList.parse)
    ; service_details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "serviceDetailSet" xml)
             ServiceDetailSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ServiceDetailSet", ServiceDetailSet.to_query v.service_details))
       ; Some
           (Aws.Query.Pair ("ServiceNameSet", ValueStringList.to_query v.service_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("serviceDetailSet", ServiceDetailSet.to_json v.service_details)
       ; Some ("serviceNameSet", ValueStringList.to_json v.service_names)
       ])

let of_json j =
  { service_names =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "serviceNameSet"))
  ; service_details =
      ServiceDetailSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "serviceDetailSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
