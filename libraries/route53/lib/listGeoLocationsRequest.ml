open Aws.BaseTypes

type t =
  { start_continent_code : String.t option
  ; start_country_code : String.t option
  ; start_subdivision_code : String.t option
  ; max_items : String.t option
  }

let make ?start_continent_code ?start_country_code ?start_subdivision_code ?max_items () =
  { start_continent_code; start_country_code; start_subdivision_code; max_items }

let parse xml =
  Some
    { start_continent_code =
        Aws.Util.option_bind (Aws.Xml.member "startcontinentcode" xml) String.parse
    ; start_country_code =
        Aws.Util.option_bind (Aws.Xml.member "startcountrycode" xml) String.parse
    ; start_subdivision_code =
        Aws.Util.option_bind (Aws.Xml.member "startsubdivisioncode" xml) String.parse
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Aws.Util.option_map v.start_subdivision_code (fun f ->
             Aws.Query.Pair ("startsubdivisioncode", String.to_query f))
       ; Aws.Util.option_map v.start_country_code (fun f ->
             Aws.Query.Pair ("startcountrycode", String.to_query f))
       ; Aws.Util.option_map v.start_continent_code (fun f ->
             Aws.Query.Pair ("startcontinentcode", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Aws.Util.option_map v.start_subdivision_code (fun f ->
             "startsubdivisioncode", String.to_json f)
       ; Aws.Util.option_map v.start_country_code (fun f ->
             "startcountrycode", String.to_json f)
       ; Aws.Util.option_map v.start_continent_code (fun f ->
             "startcontinentcode", String.to_json f)
       ])

let of_json j =
  { start_continent_code =
      Aws.Util.option_map (Aws.Json.lookup j "startcontinentcode") String.of_json
  ; start_country_code =
      Aws.Util.option_map (Aws.Json.lookup j "startcountrycode") String.of_json
  ; start_subdivision_code =
      Aws.Util.option_map (Aws.Json.lookup j "startsubdivisioncode") String.of_json
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  }
