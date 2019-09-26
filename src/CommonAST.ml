type identifier = Id  of string
type label      = Lab of string

exception Division_by_zero of (int * int)

let integer_bytes = 31
let integer_max_val = int_of_float ((2. ** (float_of_int (integer_bytes - 1))) -. 1.)
let integer_min_val = -(int_of_float ((2.) ** (float_of_int (integer_bytes - 1))))

module Symb_Tbl = Map.Make(String)
    
type typ =
  | TypVoid
  | TypInt
  | TypBool
  | TypArray of typ
  | TypStruct of string

let rec print_typ t =
  match t with
  | TypVoid -> "void"
  | TypInt -> "integer"
  | TypBool -> "boolean"
  | TypStruct(x) -> Printf.sprintf "%s" x
  | TypArray(x) -> Printf.sprintf "%s array" (print_typ x)

type struct_type = {
  fields: (string * typ) list;
}

type function_signature = {
  return: typ;
  formals: (string * typ) list;
}
  
type type_context = {
  identifier_types: typ Symb_Tbl.t;
  struct_types: struct_type Symb_Tbl.t;
  function_signatures: function_signature Symb_Tbl.t;
}

type literal =
  | Int  of int
  | Bool of bool

type unaryOp = Minus | Not
    
type binaryOp = Add | Sub | Mult | Div | Mod
                | Eq | Neq | Lt | Le | Gt | Ge
                | And | Or
