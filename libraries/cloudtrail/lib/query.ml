open Aws.BaseTypes

type t =
  { query_id : String.t option
  ; query_status : QueryStatus.t option
  ; creation_time : DateTime.t option
  }

let make ?query_id ?query_status ?creation_time () =
  { query_id; query_status; creation_time }

let parse xml =
  Some
    { query_id = Aws.Util.option_bind (Aws.Xml.member "QueryId" xml) String.parse
    ; query_status =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatus" xml) QueryStatus.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.query_status (fun f ->
             Aws.Query.Pair ("QueryStatus", QueryStatus.to_query f))
       ; Aws.Util.option_map v.query_id (fun f ->
             Aws.Query.Pair ("QueryId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.creation_time (fun f -> "CreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.query_status (fun f ->
             "QueryStatus", QueryStatus.to_json f)
       ; Aws.Util.option_map v.query_id (fun f -> "QueryId", String.to_json f)
       ])

let of_json j =
  { query_id = Aws.Util.option_map (Aws.Json.lookup j "QueryId") String.of_json
  ; query_status =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatus") QueryStatus.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
  }
