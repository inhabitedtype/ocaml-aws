open Aws.BaseTypes

type t =
  { prefixes_found : Long.t option
  ; prefixes_completed : Long.t option
  ; files_completed : Long.t option
  ; events_completed : Long.t option
  ; failed_entries : Long.t option
  }

let make
    ?prefixes_found
    ?prefixes_completed
    ?files_completed
    ?events_completed
    ?failed_entries
    () =
  { prefixes_found
  ; prefixes_completed
  ; files_completed
  ; events_completed
  ; failed_entries
  }

let parse xml =
  Some
    { prefixes_found =
        Aws.Util.option_bind (Aws.Xml.member "PrefixesFound" xml) Long.parse
    ; prefixes_completed =
        Aws.Util.option_bind (Aws.Xml.member "PrefixesCompleted" xml) Long.parse
    ; files_completed =
        Aws.Util.option_bind (Aws.Xml.member "FilesCompleted" xml) Long.parse
    ; events_completed =
        Aws.Util.option_bind (Aws.Xml.member "EventsCompleted" xml) Long.parse
    ; failed_entries =
        Aws.Util.option_bind (Aws.Xml.member "FailedEntries" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_entries (fun f ->
             Aws.Query.Pair ("FailedEntries", Long.to_query f))
       ; Aws.Util.option_map v.events_completed (fun f ->
             Aws.Query.Pair ("EventsCompleted", Long.to_query f))
       ; Aws.Util.option_map v.files_completed (fun f ->
             Aws.Query.Pair ("FilesCompleted", Long.to_query f))
       ; Aws.Util.option_map v.prefixes_completed (fun f ->
             Aws.Query.Pair ("PrefixesCompleted", Long.to_query f))
       ; Aws.Util.option_map v.prefixes_found (fun f ->
             Aws.Query.Pair ("PrefixesFound", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_entries (fun f -> "FailedEntries", Long.to_json f)
       ; Aws.Util.option_map v.events_completed (fun f ->
             "EventsCompleted", Long.to_json f)
       ; Aws.Util.option_map v.files_completed (fun f -> "FilesCompleted", Long.to_json f)
       ; Aws.Util.option_map v.prefixes_completed (fun f ->
             "PrefixesCompleted", Long.to_json f)
       ; Aws.Util.option_map v.prefixes_found (fun f -> "PrefixesFound", Long.to_json f)
       ])

let of_json j =
  { prefixes_found = Aws.Util.option_map (Aws.Json.lookup j "PrefixesFound") Long.of_json
  ; prefixes_completed =
      Aws.Util.option_map (Aws.Json.lookup j "PrefixesCompleted") Long.of_json
  ; files_completed =
      Aws.Util.option_map (Aws.Json.lookup j "FilesCompleted") Long.of_json
  ; events_completed =
      Aws.Util.option_map (Aws.Json.lookup j "EventsCompleted") Long.of_json
  ; failed_entries = Aws.Util.option_map (Aws.Json.lookup j "FailedEntries") Long.of_json
  }
