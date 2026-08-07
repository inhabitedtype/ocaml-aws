open Aws.BaseTypes

type t =
  { name : String.t option
  ; description : String.t option
  ; s_q_l : String.t option
  ; relevance : Float.t option
  }

let make ?name ?description ?s_q_l ?relevance () = { name; description; s_q_l; relevance }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; s_q_l = Aws.Util.option_bind (Aws.Xml.member "SQL" xml) String.parse
    ; relevance = Aws.Util.option_bind (Aws.Xml.member "Relevance" xml) Float.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.relevance (fun f ->
             Aws.Query.Pair ("Relevance", Float.to_query f))
       ; Aws.Util.option_map v.s_q_l (fun f -> Aws.Query.Pair ("SQL", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.relevance (fun f -> "Relevance", Float.to_json f)
       ; Aws.Util.option_map v.s_q_l (fun f -> "SQL", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; s_q_l = Aws.Util.option_map (Aws.Json.lookup j "SQL") String.of_json
  ; relevance = Aws.Util.option_map (Aws.Json.lookup j "Relevance") Float.of_json
  }
