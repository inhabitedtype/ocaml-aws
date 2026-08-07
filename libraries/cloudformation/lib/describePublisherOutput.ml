open Aws.BaseTypes

type t =
  { publisher_id : String.t option
  ; publisher_status : PublisherStatus.t option
  ; identity_provider : IdentityProvider.t option
  ; publisher_profile : String.t option
  }

let make ?publisher_id ?publisher_status ?identity_provider ?publisher_profile () =
  { publisher_id; publisher_status; identity_provider; publisher_profile }

let parse xml =
  Some
    { publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    ; publisher_status =
        Aws.Util.option_bind (Aws.Xml.member "PublisherStatus" xml) PublisherStatus.parse
    ; identity_provider =
        Aws.Util.option_bind
          (Aws.Xml.member "IdentityProvider" xml)
          IdentityProvider.parse
    ; publisher_profile =
        Aws.Util.option_bind (Aws.Xml.member "PublisherProfile" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.publisher_profile (fun f ->
             Aws.Query.Pair ("PublisherProfile", String.to_query f))
       ; Aws.Util.option_map v.identity_provider (fun f ->
             Aws.Query.Pair ("IdentityProvider", IdentityProvider.to_query f))
       ; Aws.Util.option_map v.publisher_status (fun f ->
             Aws.Query.Pair ("PublisherStatus", PublisherStatus.to_query f))
       ; Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.publisher_profile (fun f ->
             "PublisherProfile", String.to_json f)
       ; Aws.Util.option_map v.identity_provider (fun f ->
             "IdentityProvider", IdentityProvider.to_json f)
       ; Aws.Util.option_map v.publisher_status (fun f ->
             "PublisherStatus", PublisherStatus.to_json f)
       ; Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f)
       ])

let of_json j =
  { publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json
  ; publisher_status =
      Aws.Util.option_map (Aws.Json.lookup j "PublisherStatus") PublisherStatus.of_json
  ; identity_provider =
      Aws.Util.option_map (Aws.Json.lookup j "IdentityProvider") IdentityProvider.of_json
  ; publisher_profile =
      Aws.Util.option_map (Aws.Json.lookup j "PublisherProfile") String.of_json
  }
