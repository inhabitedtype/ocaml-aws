open OUnit
open Aws_sqs

let from_opt = function
  | None -> assert false
  | Some(x) -> x

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
      region:string
      -> (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let create_delete_queue_test () =
    let res = Runtime.(un_m (run_request
                               ~region:"us-east-1"
                               (module CreateQueue)
                               (Types.CreateQueueRequest.make ~queue_name:"test_queue"()))) in
    "Create Queue"
    @? begin match res with
       | `Ok resp ->
          Printf.printf "%s\n" (Yojson.Basic.to_string (Types.CreateQueueResult.(to_json (of_json (to_json resp)))));
          true
       | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
       end;
    let queue_url = match res with
      | `Ok resp ->
         resp.queue_url
      | `Error err -> assert false
    in

    let delete_res = Runtime.(un_m (run_request
                             ~region:"us-east-1"
                             (module DeleteQueue)
                             (Types.DeleteQueueRequest.make ~queue_url:(from_opt queue_url) ()))) in

    "Delete Queue"
    @? begin match delete_res with
       | `Ok resp ->
          true
       | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
       end

  let test_cases =
    [ "SQS create / delete queue" >:: create_delete_queue_test ]

  let rec was_successful =
    function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
      was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
      false
  let _ =
    let suite = "Tests" >::: test_cases in
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
      exit 1
end
