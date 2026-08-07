open Aws.BaseTypes

type t =
  { continent_code : String.t option
  ; country_code : String.t option
  ; subdivision_code : String.t option
  }

let make ?continent_code ?country_code ?subdivision_code () =
  { continent_code; country_code; subdivision_code }

let parse xml =
  Some
    { continent_code =
        Aws.Util.option_bind (Aws.Xml.member "continentcode" xml) String.parse
    ; country_code = Aws.Util.option_bind (Aws.Xml.member "countrycode" xml) String.parse
    ; subdivision_code =
        Aws.Util.option_bind (Aws.Xml.member "subdivisioncode" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subdivision_code (fun f ->
             Aws.Query.Pair ("subdivisioncode", String.to_query f))
       ; Aws.Util.option_map v.country_code (fun f ->
             Aws.Query.Pair ("countrycode", String.to_query f))
       ; Aws.Util.option_map v.continent_code (fun f ->
             Aws.Query.Pair ("continentcode", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subdivision_code (fun f ->
             "subdivisioncode", String.to_json f)
       ; Aws.Util.option_map v.country_code (fun f -> "countrycode", String.to_json f)
       ; Aws.Util.option_map v.continent_code (fun f -> "continentcode", String.to_json f)
       ])

let of_json j =
  { continent_code =
      Aws.Util.option_map (Aws.Json.lookup j "continentcode") String.of_json
  ; country_code = Aws.Util.option_map (Aws.Json.lookup j "countrycode") String.of_json
  ; subdivision_code =
      Aws.Util.option_map (Aws.Json.lookup j "subdivisioncode") String.of_json
  }
