open Aws.BaseTypes

type t =
  { id : String.t
  ; sender_fault : Boolean.t
  ; code : String.t
  ; message : String.t option
  }

let make ~id ~sender_fault ~code ?message () = { id; sender_fault; code; message }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; sender_fault =
        Aws.Xml.required
          "SenderFault"
          (Aws.Util.option_bind (Aws.Xml.member "SenderFault" xml) Boolean.parse)
    ; code =
        Aws.Xml.required
          "Code"
          (Aws.Util.option_bind (Aws.Xml.member "Code" xml) String.parse)
    ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ; Some (Aws.Query.Pair ("Code", String.to_query v.code))
       ; Some (Aws.Query.Pair ("SenderFault", Boolean.to_query v.sender_fault))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
       ; Some ("Code", String.to_json v.code)
       ; Some ("SenderFault", Boolean.to_json v.sender_fault)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; sender_fault =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SenderFault"))
  ; code = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Code"))
  ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
  }
