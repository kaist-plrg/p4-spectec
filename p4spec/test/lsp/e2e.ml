module Json = Yojson.Safe.Util

let check condition message = if not condition then failwith message

let request id method_ params =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String method_);
      ("params", params);
    ]

let notification method_ params =
  `Assoc
    [
      ("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params);
    ]

let send channel message =
  let body = Yojson.Safe.to_string message in
  Printf.fprintf channel "Content-Length: %d\r\n\r\n%s" (String.length body)
    body;
  flush channel

let receive channel =
  let rec content_length length =
    match String.trim (input_line channel) with
    | "" -> length
    | header -> (
        match String.split_on_char ':' header with
        | [ name; value ]
          when String.lowercase_ascii (String.trim name) = "content-length" ->
            content_length (Some (int_of_string (String.trim value)))
        | name :: _
          when String.lowercase_ascii (String.trim name) = "content-type" ->
            content_length length
        | _ -> failwith ("unexpected protocol output: " ^ header))
  in
  match content_length None with
  | None -> failwith "missing Content-Length header"
  | Some length -> Yojson.Safe.from_string (really_input_string channel length)

let response channel id =
  let reply = receive channel in
  check
    (Json.member "id" reply = `Int id && Json.member "error" reply = `Null)
    ("unexpected response: " ^ Yojson.Safe.to_string reply);
  Json.member "result" reply

let publication channel uri =
  let reply = receive channel in
  check
    (Json.member "method" reply = `String "textDocument/publishDiagnostics")
    ("unexpected notification: " ^ Yojson.Safe.to_string reply);
  let params = Json.member "params" reply in
  check
    (Json.member "uri" params = `String uri)
    ("published to a different URI: " ^ Yojson.Safe.to_string reply);
  Json.to_list (Json.member "diagnostics" params)

let did_open uri text =
  notification "textDocument/didOpen"
    (`Assoc
      [
        ( "textDocument",
          `Assoc
            [
              ("uri", `String uri);
              ("languageId", `String "watsup");
              ("version", `Int 1);
              ("text", `String text);
            ] );
      ])

let did_change uri version change =
  notification "textDocument/didChange"
    (`Assoc
      [
        ( "textDocument",
          `Assoc [ ("uri", `String uri); ("version", `Int version) ] );
        ("contentChanges", `List [ change ]);
      ])

let did_close uri =
  notification "textDocument/didClose"
    (`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])

let with_server server_bin f =
  let from_server_r, from_server_w = Unix.pipe ~cloexec:true () in
  let to_server_r, to_server_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process server_bin [| server_bin |] to_server_r from_server_w
      Unix.stderr
  in
  Unix.close to_server_r;
  Unix.close from_server_w;
  let from_server = Unix.in_channel_of_descr from_server_r in
  let to_server = Unix.out_channel_of_descr to_server_w in
  let reaped = ref false in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr from_server;
      close_out_noerr to_server;
      if not !reaped then (
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid)))
    (fun () ->
      f to_server from_server;
      close_out to_server;
      let _, status = Unix.waitpid [] pid in
      reaped := true;
      check (status = Unix.WEXITED 0) "server did not exit successfully")

let exercise server_bin encoding expected_column =
  with_server server_bin (fun to_server from_server ->
      let general =
        match encoding with
        | "utf-16" -> []
        | _ ->
            [
              ( "general",
                `Assoc [ ("positionEncodings", `List [ `String encoding ]) ] );
            ]
      in
      send to_server
        (request 1 "initialize" (`Assoc [ ("capabilities", `Assoc general) ]));
      let capabilities = Json.member "capabilities" (response from_server 1) in
      check
        (Json.member "positionEncoding" capabilities = `String encoding)
        "position encoding was not negotiated";
      check
        (Json.member "change" (Json.member "textDocumentSync" capabilities)
        = `Int 2)
        "incremental synchronization was not advertised";
      send to_server (notification "initialized" (`Assoc []));
      let uri = "file:///lsp%20test.watsup" in
      send to_server (did_open uri "(; 😀é ;) @\n");
      let diagnostic =
        match publication from_server uri with
        | [ diagnostic ] -> diagnostic
        | _ -> failwith "opening the buffer did not publish one diagnostic"
      in
      let start = Json.member "start" (Json.member "range" diagnostic) in
      check
        (Json.member "character" start = `Int expected_column)
        "diagnostic range uses the wrong encoding";
      let position column =
        `Assoc [ ("line", `Int 0); ("character", `Int column) ]
      in
      send to_server
        (did_change uri 2
           (`Assoc
             [
               ( "range",
                 `Assoc
                   [
                     ("start", position expected_column);
                     ("end", position (expected_column + 1));
                   ] );
               ("text", `String "");
             ]));
      check
        (publication from_server uri = [])
        "incremental repair did not clear the error";
      send to_server (did_change uri 3 (`Assoc [ ("text", `String "@") ]));
      check
        (List.length (publication from_server uri) = 1)
        "parser error was not published";
      send to_server (did_close uri);
      check
        (publication from_server uri = [])
        "closing the document did not clear diagnostics";
      send to_server (request 2 "shutdown" `Null);
      check (response from_server 2 = `Null) "invalid shutdown response";
      send to_server (notification "exit" `Null));
  Printf.printf
    "%s: initialize, open, incremental edit, repair, close, shutdown, exit: ok\n"
    encoding

let () =
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ -> failwith "LSP test timed out"));
  ignore (Unix.alarm 20);
  exercise Sys.argv.(1) "utf-16" 10;
  exercise Sys.argv.(1) "utf-8" 13;
  ignore (Unix.alarm 0)
