open Aws.BaseTypes

type t = { directory_id : String.t option }

let make ?directory_id () = { directory_id }

let parse xml =
  Some
    { directory_id = Aws.Util.option_bind (Aws.Xml.member "DirectoryId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.directory_id (fun f ->
             Aws.Query.Pair ("DirectoryId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.directory_id (fun f -> "DirectoryId", String.to_json f) ])

let of_json j =
  { directory_id = Aws.Util.option_map (Aws.Json.lookup j "DirectoryId") String.of_json }
