open Aws.BaseTypes

type t =
  { start_date : DateTime.t option
  ; end_date : DateTime.t option
  ; value : Float.t option
  ; status : String.t option
  }

let make ?start_date ?end_date ?value ?status () = { start_date; end_date; value; status }

let parse xml =
  Some
    { start_date = Aws.Util.option_bind (Aws.Xml.member "startDate" xml) DateTime.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "endDate" xml) DateTime.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "value" xml) Float.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.value (fun f -> Aws.Query.Pair ("Value", Float.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Aws.Util.option_map v.value (fun f -> "value", Float.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "endDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "startDate", DateTime.to_json f)
       ])

let of_json j =
  { start_date = Aws.Util.option_map (Aws.Json.lookup j "startDate") DateTime.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "endDate") DateTime.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "value") Float.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  }
