open Lwt.Infix
open Notty

let default_start = 0x2580

let draw_16_block starting_at =
  let column n =
    let display = try
        I.uchar A.empty (Uchar.of_int n ) 1 1
      with | Invalid_argument _ -> I.void 1 1
    in
    I.vcat [I.string A.empty (Printf.sprintf "%x " (n mod 16)) ;
            display ]
  in
  let uchars = List.init 16 (fun i -> column (i + starting_at)) in
  let header = I.string A.empty (Printf.sprintf "0x%x" starting_at) in
  let open Notty.Infix in
  header <->
  I.hcat uchars

let check_edge n =
  if n < Uchar.(to_int min) then Uchar.(to_int min)
  else if n > Uchar.(to_int max) then (Uchar.(to_int max) mod 16)
  else n

let next_page_start starting_at height =
  let how_many = height / 4 in
  let prospective = starting_at + ((how_many + 1)* 16) in
  check_edge prospective

let prev_page_start starting_at height =
  let how_many = height / 4 in
  let prospective = starting_at  - (how_many * 16) in
  check_edge prospective

let draw_page starting_at (_, height) =
  let how_many = height / 4 in
  Notty.I.vcat @@ 
    List.init how_many (fun i -> draw_16_block @@ starting_at + (i * 16))

let process_event prev_start (width, height) event =
  let redraw = Some (draw_page prev_start (width, height), prev_start) in
  match event with
  | `Resize (width, height) -> Some (draw_page prev_start (width, height), prev_start)
  | `Key (key, mods) -> begin
      let is_shifted = List.mem `Shift mods in
    match key with
    | `Arrow `Left | `ASCII 'h' ->
      let next_from = prev_page_start prev_start height in
      Some (draw_page (prev_page_start prev_start height) (width, height), next_from)
    | `Arrow `Right | `ASCII 'l' ->
      let next_from = next_page_start prev_start height in
      Some (draw_page (next_page_start prev_start height) (width, height), next_from)
    | `Arrow `Down | `ASCII 'j' ->
      let amount = if is_shifted then 0x1000 else 0x100 in
      let next_from = check_edge @@
        (prev_start - (prev_start mod amount) + amount) in
      Some (draw_page next_from (width, height), next_from)
    | `Arrow `Up | `ASCII 'k' ->
      let next_from = check_edge @@
      let amount = if is_shifted then 0x1000 else 0x100 in
        (prev_start - (prev_start mod amount) - amount) in
      Some (draw_page next_from (width, height), next_from)
    | `Escape | `ASCII 'q' -> None
    | _ -> redraw
  end
  | _ -> redraw

let rec loop term from =
  Lwt_stream.last_new (Notty_lwt.Term.events term) >>= fun event ->
  match process_event from (Notty_lwt.Term.size term) event with
  | None -> Notty_lwt.Term.release term
  | Some (image, next_from) ->
    Notty_lwt.Term.image term image >>= fun () ->
    loop term next_from

let () =
  let term = Notty_lwt.Term.create () in
  Lwt_main.run @@ (
    Notty_lwt.Term.image term (draw_page default_start (Notty_lwt.Term.size term)) >>= fun () ->
    loop term default_start >>= fun () ->
    Notty_lwt.Term.release term)
