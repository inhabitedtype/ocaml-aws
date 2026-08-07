open Aws.BaseTypes

type t = { registration_token : String.t option }

let make ?registration_token () = { registration_token }

let parse xml =
  Some
    { registration_token =
        Aws.Util.option_bind (Aws.Xml.member "RegistrationToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.registration_token (fun f ->
             Aws.Query.Pair ("RegistrationToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.registration_token (fun f ->
             "RegistrationToken", String.to_json f)
       ])

let of_json j =
  { registration_token =
      Aws.Util.option_map (Aws.Json.lookup j "RegistrationToken") String.of_json
  }
