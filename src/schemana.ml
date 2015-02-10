open Core.Std
open MParser (* Note: MParser overwrites Core.Std operator <|> *)

let symbol:(char, unit) MParser.t = MParser.any_of "!#$%&|*+-/:<=>?@^_~"

let spaces':(unit, unit) MParser.t = MParser.skip_many1 MParser.space

type lispVal =
	| Atom of string
	| List of lispVal list
	| DottedList of (lispVal list) * lispVal
	| Number of int
	| String of string
	| Bool of bool
	| PrimitiveFunc of (lispVal list -> lispVal)
	| Func of lispFunc
and lispEnv = (string * lispVal) list
and lispFunc = { params:string list; vararg: string option; body:(lispVal list); closure: lispEnv ref }

let isBound (env:lispEnv ref) (name:string):bool = 
	List.filter ~f:(fun (_name, _) -> _name = name) !env 
	|> List.length > 0

let getVar (env:lispEnv ref) (name:string):lispVal =
	match List.find ~f:(fun (_name, _value) -> _name = name) !env with 
		| Some (__name, __value) -> __value 
		| None -> raise (Invalid_argument ("getVar: no variable named: " ^ name))

let setVar (env:lispEnv ref) (name:string) (value:lispVal):unit =
	env := List.filter ~f:(fun (_name, _) -> _name <> name) !env;
	env := (name, value) :: !env;
	()

let bindVars (env:lispEnv ref) (vars:lispEnv):(lispEnv ref) = 
	List.iter ~f:(fun (name, value) -> setVar env name value) vars; (* TODO: setVar should return lispVal; -> ( block ) might solve *)
	env

let parseString:(lispVal, unit) MParser.t =	MParser.char '"' 
	>> MParser.many @@ MParser.none_of "\"" 
	>>= fun s -> MParser.char '"' 
		>> return @@ String (String.of_char_list s)

let parseAtom:(lispVal, unit) MParser.t = (MParser.letter <|> symbol) 
	>>= fun first -> MParser.many (MParser.letter <|> MParser.digit <|> symbol) 
		>>=	fun last ->	let atom = String.of_char first ^ String.of_char_list last in
			return @@ match atom with
				| "#t" -> Bool true
				| "#f" -> Bool false
				| _ -> Atom atom

let parseNumber:(lispVal, unit) MParser.t =	MParser.many1 MParser.digit 
	>>= fun digits -> return @@ Number (Int.of_string @@ String.of_char_list digits)

let rec parseExpr:(lispVal, unit) MParser.t lazy_t = lazy (
	parseAtom
	<|> parseString
	<|> parseNumber
	<|> attempt (force parseList)
	<|> force parseDottedList
) and parseList = lazy (
	MParser.char '(' 
	>>= fun _ -> MParser.sep_by (force parseExpr) spaces' 
	>>= fun l -> List l |> return
	>>= fun item -> MParser.char ')' >> return item
) and parseDottedList = lazy (
	MParser.char '('
	>>= fun _ -> MParser.end_by (force parseExpr) spaces'
  	>>= fun head ->	MParser.char '.' >> spaces' >> MParser.many1 (force parseExpr)
	>>= fun tail -> MParser.char ')' >>	return (DottedList (head, (List.last_exn tail) ))
)

let rec showVal:lispVal -> bytes = function
	| String s -> "\"" ^ s ^ "\""
	| Atom s -> s
	| Number n -> string_of_int n
	| List l -> "(" ^ unwordList l ^ ")"
	| DottedList (head, last) -> "(" ^ unwordList head ^ " . " ^ (showVal last) ^ ")"
	| Bool true -> "#t"
	| Bool false -> "#f"
	| PrimitiveFunc _ -> "<primitive>"
	| Func _ -> "<function>"
and unwordList (list:lispVal list):bytes = List.map ~f:(fun item -> showVal item) list |> String.concat ~sep:" "

let readExpr (input:bytes):lispVal = match parse_string (force parseExpr) input () with
	| Success v -> v
	| Failed (msg, _) -> (fun _ -> raise (Invalid_argument ("LISP needs LISTS: " ^ input ^ "\n" ^ msg))) (String "Error")

let car:(lispVal list -> lispVal) = function
	| [List (x :: _)] -> x
	| [_] -> raise (Invalid_argument ("car: Type Mismatch pair"))
	| _ -> raise (Invalid_argument ("car: Number of arguments"))

let cdr:(lispVal list -> lispVal) = function
	| [List (_ :: xs)] -> List xs
	| [_] -> raise (Invalid_argument ("cdr: Type Mismatch pair"))
	| _ -> raise (Invalid_argument ("cdr: Number of arguments"))

let cons:(lispVal list -> lispVal) = function
	| [x; List []] -> List [x]
	| [x; List xs] -> List (x :: xs)
	| _ -> raise (Invalid_argument ("cons: Number of arguments"))

let eqv:(lispVal list -> lispVal) = function
	| [Bool arg1; Bool arg2] -> Bool (arg1 = arg2)
	| [Number arg1; Number arg2] -> Bool (arg1 = arg2)
	| [String arg1; String arg2] -> Bool (arg1 = arg2)
	| [Atom arg1; Atom arg2] -> Bool (arg1 = arg2)
	| [List arg1; List arg2] -> Bool (List.length arg1 = List.length arg2) (* TODO *)
	| [_;_] -> Bool false
	| _ -> raise (Invalid_argument ("eqv: Number of arguments"))

let rec unpackNum = function
	| Number n -> n
	| List [n] -> unpackNum n
	| _ -> raise (Invalid_argument ("unpackNum: Not a number or list of numbers"))

let numOp (op:int -> int -> int) (params:lispVal list):lispVal = 
	Number (match (List.reduce ~f:(op) (List.map ~f:(unpackNum) params)) with None -> 0|Some n -> n)

let unpackBool = function
	| Bool b -> b
	| _ -> raise (Invalid_argument ("unpackBool: Type Mismatch: bool operation needs bools"))

let boolOp (unpacker:lispVal -> 'a) (op:'a -> 'a -> bool) (params:lispVal list):lispVal =
	if List.length params <> 2 then (fun _ -> raise (Invalid_argument ("boolOp: bool operation needs two arguments"))) (String "Error")
	else Bool (op (unpacker @@ List.nth_exn params 0) (unpacker @@ List.nth_exn params 1))

let unpackString = function
	| String s -> s
	| Number s -> string_of_int s
	| Bool s -> string_of_bool s
	| _ -> raise (Invalid_argument ("unpackString: Type Mismatch: not a string"))

let primitiveBindings (env:lispEnv ref) = 
	let primitives = [
		("+", numOp (+));
		("-", numOp (-));
		("*", numOp ( * ));
		("/", numOp (/));
		("%", numOp (mod));
		("=", boolOp unpackNum (=));
		("!=", boolOp unpackNum (<>));
		("<", boolOp unpackNum (<));
		(">", boolOp unpackNum (>));
		(">=", boolOp unpackNum (>=));
		("<=", boolOp unpackNum (<=));
		("&&", boolOp unpackBool (&&));
		("||", boolOp unpackBool (||));
		("string=?", boolOp unpackString (=));
		("string<?", boolOp unpackString (<));
		("string>?", boolOp unpackString (>));
		("string<=?", boolOp unpackString (<=));
		("string>=?", boolOp unpackString (>=));
		("car", car);
		("cdr", cdr);
		("cons", cons);
		("eq?", eqv);
		("eqv?", eqv)]
	in
		bindVars env (List.map ~f:(fun p ->
			match p with
			| (name, func) -> name, PrimitiveFunc func
			) primitives )

let makeFunc (vararg:string option) (env:lispEnv ref) (params:lispVal list) (body:lispVal list):lispVal =
	Func {
		params = List.map ~f:showVal params;
		vararg; 
		body; 
		closure = env 
	}

let makeNormalFunc = makeFunc None

let makeVarArgs x = makeFunc (Some x)

let rec eval (env:lispEnv ref) (expr:lispVal):lispVal =
	match expr with
		| Atom name -> getVar env name
		| List [Atom "if"; predicate; consequence; alternative] ->
			(match (eval env predicate) with
				| Bool false -> eval env alternative
				| _ -> eval env consequence
			)
		| List [Atom "set!"; Atom name; form] | List [Atom "define"; Atom name; form] ->
			setVar env name (eval env form); 
			getVar env name
		| List ((Atom "define") :: List (Atom name :: params) :: body) ->
			makeNormalFunc env params body
			|> setVar env name
			|> fun _ -> getVar env name
		| List ((Atom "lambda") :: List params :: body) -> 
			makeNormalFunc env params body
 		| List (func :: args) -> apply (eval env func) @@ List.map ~f:(eval env) args
		| v -> v
and apply (func:lispVal) (args:lispVal list):lispVal =
	match func with
		| PrimitiveFunc func -> func args
		| Func { params = _params; vararg = _vararg; body = _body; closure = _closure } ->
			if List.length _params <> List.length args && _vararg = None then
				raise (Invalid_argument ("apply Func: Type Mismatch: Func"))
			else
				let bindVarArgs arg env =
					match arg with
						| None -> env
						| Some argName -> bindVars env [( argName, List (List.drop args (List.length _params)) )]
				and evalBody env = 
					List.last_exn (List.map ~f:(eval (ref (List.slice !env 0 0))) _body)
				in
					bindVars _closure (List.zip_exn _params args) 
					|> bindVarArgs _vararg
					|> evalBody
		| _ -> raise (Invalid_argument ("apply Func: Type Mismatch: Not a PrimitiveFunc nor Func"))

let repl env = lazy(
	let end_of_file = ref false in
		while !end_of_file = false do
			try
				printf "SCHEMana >>> ";
				let scheme_expression = read_line () in
					showVal (eval env @@ readExpr scheme_expression) ^ "\n" |> print_string
			with 
				| End_of_file -> end_of_file := true; printf "\n"
				| Invalid_argument e -> printf "ERR: %s\n" e
		done
)

let () =
	let (env:lispEnv ref) = primitiveBindings (ref []) in
		if Array.length Sys.argv <= 1 then
			force (repl env)
		else
			let scheme_expression =	Sys.argv.(1) in
		    	showVal (eval env @@ readExpr scheme_expression) ^ "\n" |> print_string
