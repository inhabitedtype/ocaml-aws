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
        Aws.Util.option_bind (Aws.Xml.member "ContinentCode" xml) String.parse
    ; country_code = Aws.Util.option_bind (Aws.Xml.member "CountryCode" xml) String.parse
    ; subdivision_code =
        Aws.Util.option_bind (Aws.Xml.member "SubdivisionCode" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subdivision_code (fun f ->
             Aws.Query.Pair ("SubdivisionCode", String.to_query f))
       ; Aws.Util.option_map v.country_code (fun f ->
             Aws.Query.Pair ("CountryCode", String.to_query f))
       ; Aws.Util.option_map v.continent_code (fun f ->
             Aws.Query.Pair ("ContinentCode", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subdivision_code (fun f ->
             "SubdivisionCode", String.to_json f)
       ; Aws.Util.option_map v.country_code (fun f -> "CountryCode", String.to_json f)
       ; Aws.Util.option_map v.continent_code (fun f -> "ContinentCode", String.to_json f)
       ])

let of_json j =
  { continent_code =
      Aws.Util.option_map (Aws.Json.lookup j "ContinentCode") String.of_json
  ; country_code = Aws.Util.option_map (Aws.Json.lookup j "CountryCode") String.of_json
  ; subdivision_code =
      Aws.Util.option_map (Aws.Json.lookup j "SubdivisionCode") String.of_json
  }
