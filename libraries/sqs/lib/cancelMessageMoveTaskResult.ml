open Aws.BaseTypes

type t = { approximate_number_of_messages_moved : Long.t option }

let make ?approximate_number_of_messages_moved () =
  { approximate_number_of_messages_moved }

let parse xml =
  Some
    { approximate_number_of_messages_moved =
        Aws.Util.option_bind
          (Aws.Xml.member "ApproximateNumberOfMessagesMoved" xml)
          Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.approximate_number_of_messages_moved (fun f ->
             Aws.Query.Pair ("ApproximateNumberOfMessagesMoved", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.approximate_number_of_messages_moved (fun f ->
             "ApproximateNumberOfMessagesMoved", Long.to_json f)
       ])

let of_json j =
  { approximate_number_of_messages_moved =
      Aws.Util.option_map
        (Aws.Json.lookup j "ApproximateNumberOfMessagesMoved")
        Long.of_json
  }
