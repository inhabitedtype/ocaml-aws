open OUnit
open Aws_sqs

let from_opt = function
  | None -> OUnit.assert_failure "from_opt is None!"
  | Some x -> x

module TestSuite (Runtime : sig
  type 'a m

  val run_request :
       (module Aws.Call
          with type input = 'input
           and type output = 'output
           and type error = 'error)
    -> 'input
    -> [ `Ok of 'output | `Error of 'error Aws.Error.t ] m

  val un_m : 'a m -> 'a
end) =
struct
  let delete_queue queue_url =
    Runtime.(
      un_m
        (run_request (module DeleteQueue) (Types.DeleteQueueRequest.make ~queue_url ())))

  (* TODO Tag created queues with project name for later cleanup / tracking *)
  let create_queue queue_name =
    Runtime.(
      un_m
        (run_request (module CreateQueue) (Types.CreateQueueRequest.make ~queue_name ())))

  let send_message queue_url message_body =
    Runtime.(
      un_m
        (run_request
           (module SendMessage)
           (Types.SendMessageRequest.make ~queue_url ~message_body ())))

  let receive_message queue_url =
    Runtime.(
      un_m
        (run_request
           (module ReceiveMessage)
           (Types.ReceiveMessageRequest.make ~queue_url ())))

  let list_queues ~queue_name_prefix =
    Runtime.(
      un_m
        (run_request
           (module ListQueues)
           (Types.ListQueuesRequest.make ~queue_name_prefix ())))

  let print_messages (msg : Types.MessageList.t) =
    List.iter
      (function
        | (x : Types.Message.t) -> Printf.printf "Message: %s\n" (from_opt x.body))
      msg

  let send_receive_message_test () =
    let test_message = "Information about the largest city in Any Region." in
    let queue_name = "aws-sqs-test-2" in
    let create_res = create_queue queue_name in
    ("SQS create queue"
    @?
    match create_res with
    | `Ok resp ->
        Printf.printf
          "Queue created %s\n"
          (Yojson.Basic.to_string
             Types.CreateQueueResult.(to_json (of_json (to_json resp))));
        true
    | `Error err ->
        Printf.printf
          "Create Error: %s\n"
          (Aws.Error.format Errors_internal.to_string err);
        false);

    let queue_url =
      match create_res with
      | `Ok resp -> from_opt resp.queue_url
      | `Error err ->
          Printf.printf
            "Create Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          OUnit.assert_failure "no queue_url for you!"
    in
    let send_message = send_message queue_url test_message in
    ("SQS send message"
    @?
    match send_message with
    | `Ok resp ->
        Printf.printf "Sending message %s\n" test_message;
        true
    | `Error err ->
        Printf.printf
          "Sending Error: %s\n"
          (Aws.Error.format Errors_internal.to_string err);
        false);

    let receive_message = receive_message queue_url in
    ("SQS receive message"
    @?
    match receive_message with
    | `Ok resp ->
        Printf.printf "Receiving messages\n";
        print_messages resp.messages;
        true
    | `Error err ->
        Printf.printf
          "Receiving Error: %s\n"
          (Aws.Error.format Errors_internal.to_string err);
        false);

    let delete_res = delete_queue queue_url in
    "Delete queue"
    @?
    match delete_res with
    | `Ok resp ->
        Printf.printf "Queue %s deleted\n" queue_name;
        true
    | `Error err ->
        Printf.printf
          "Delete Error: %s\n"
          (Aws.Error.format Errors_internal.to_string err);
        false

  let create_delete_queue_test () =
    let queue_name = "aws-sqs-test-1" in
    let create_res = create_queue queue_name in
    ("SQS create / delete queue"
    @?
    match create_res with
    | `Ok resp ->
        Printf.printf
          "%s\n"
          (Yojson.Basic.to_string
             Types.CreateQueueResult.(to_json (of_json (to_json resp))));
        true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false);
    let queue_url =
      match create_res with
      | `Ok resp -> from_opt resp.queue_url
      | `Error err -> assert false
    in
    let delete_res = delete_queue queue_url in
    "Delete queue"
    @?
    match delete_res with
    | `Ok resp ->
        Printf.printf "Queue %s deleted\n" queue_name;
        true
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false

  let create_list_queue_test () =
    let queue_name_prefix = "aws-sqs-test-0" in
    let _ = create_queue queue_name_prefix in
    let list_queue_res = list_queues ~queue_name_prefix in
    "SQS create many queues and list"
    @?
    match list_queue_res with
    | `Ok resp ->
        Printf.printf
          "List queues: %s\n"
          (Yojson.Basic.to_string (Types.QueueUrlList.to_json resp.queue_urls));
        let _ = List.map delete_queue resp.queue_urls in
        List.length resp.queue_urls == 1
    | `Error err ->
        Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
        false

  let test_cases =
    [ "SQS create / delete queue" >:: create_delete_queue_test
    ; "SQS create / list / delete queue" >:: create_list_queue_test
    ; "SQS send / receive message" >:: send_receive_message_test
    ]

  let rec was_successful = function
    | [] -> true
    | RSuccess _ :: t | RSkip _ :: t -> was_successful t
    | RFailure _ :: _ | RError _ :: _ | RTodo _ :: _ -> false

  let _ =
    let suite = "Tests" >::: test_cases in
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [ "-verbose", Arg.Unit set_verbose, "Run the test in verbose mode." ]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (run_test_tt ~verbose:!verbose suite)) then exit 1
end
