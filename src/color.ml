(** This file contains string for color output in stdout 
 ⚠: when used in a file creates buggy logs *)


let reset   = "\027[0m"
(** reset all formatting, colors & style *)

(** {5 Colors} *)

let grey    = "\027[2m"
let dgrey   = "\027[30m"
let red     = "\027[31m"
let green   = "\027[32m"
let yellow  = "\027[33m"
let blue    = "\027[34m"
let purple  = "\027[35m"
let cyen    = "\027[36m"

(** {5 Style} *)

let bold    = "\027[1m"
let italic  = "\027[3m"
let uline   = "\027[4m"

let uuline  = "\027[21m"  (** Double underlined text *)

let blink   = "\027[5m"   (** Blinking text *)

let backgrd = "\027[7m"   (** reverse font & background color *)

let dashed  = "\027[9m"

(** {5 Utils} *)

let sign = Printf.sprintf "%s%s⚠ %s" red blink reset
(** red blinking ⚠ symbol *)

let tick = Printf.sprintf "%s%s✓%s" green bold reset
let cross = Printf.sprintf "%s%s✕%s" red bold reset

(*
let ao = Printf.sprintf "%s[%s" green reset
let af = Printf.sprintf "%s]%s" green reset
let bc = Printf.sprintf "%s|%s" green reset
*)

(* let ao = Printf.sprintf "[" *)
(* let af = Printf.sprintf "]" *)
(* let bc = Printf.sprintf "|" *)

 let ao, af, bc = "", "", ""

type effect =
  | Reset
  | Dgrey
  | Grey
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Cyan
  | Bold
  | Italic
  | Uline
  | UUline
  | Backgrd
  | Dashed

let print_effect apply fmt (effect: effect) =
  if not (!Options.color && apply) then () else
  Format.fprintf fmt "%s"
    (match effect with
    | Reset -> reset
    | Dgrey -> dgrey
    | Grey -> grey
    | Red -> red
    | Green -> green
    | Yellow -> yellow
    | Blue -> blue
    | Purple -> purple
    | Cyan -> cyen
    | Bold -> bold
    | Italic -> italic
    | Uline -> uline
    | UUline -> uuline
    | Backgrd -> backgrd
    | Dashed -> dashed)

