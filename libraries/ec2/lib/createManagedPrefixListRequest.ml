open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; prefix_list_name : String.t
  ; entries : AddPrefixListEntries.t
  ; max_entries : Integer.t
  ; tag_specifications : TagSpecificationList.t
  ; address_family : String.t
  ; client_token : String.t option
  }

let make
    ?dry_run
    ~prefix_list_name
    ?(entries = [])
    ~max_entries
    ?(tag_specifications = [])
    ~address_family
    ?client_token
    () =
  { dry_run
  ; prefix_list_name
  ; entries
  ; max_entries
  ; tag_specifications
  ; address_family
  ; client_token
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; prefix_list_name =
        Aws.Xml.required
          "PrefixListName"
          (Aws.Util.option_bind (Aws.Xml.member "PrefixListName" xml) String.parse)
    ; entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Entry" xml) AddPrefixListEntries.parse)
    ; max_entries =
        Aws.Xml.required
          "MaxEntries"
          (Aws.Util.option_bind (Aws.Xml.member "MaxEntries" xml) Integer.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; address_family =
        Aws.Xml.required
          "AddressFamily"
          (Aws.Util.option_bind (Aws.Xml.member "AddressFamily" xml) String.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("AddressFamily", String.to_query v.address_family))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("MaxEntries", Integer.to_query v.max_entries))
       ; Some (Aws.Query.Pair ("Entry", AddPrefixListEntries.to_query v.entries))
       ; Some (Aws.Query.Pair ("PrefixListName", String.to_query v.prefix_list_name))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("AddressFamily", String.to_json v.address_family)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("MaxEntries", Integer.to_json v.max_entries)
       ; Some ("Entry", AddPrefixListEntries.to_json v.entries)
       ; Some ("PrefixListName", String.to_json v.prefix_list_name)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; prefix_list_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrefixListName"))
  ; entries =
      AddPrefixListEntries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Entry"))
  ; max_entries =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxEntries"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; address_family =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AddressFamily"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
