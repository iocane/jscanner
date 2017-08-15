open Token
       
type scanner = {buf: Buffer.t; mutable offset: int; mutable next_offset: int; mutable ch: char}

let read_file_into_buffer f = 
  let bs = Bytes.create 512 in
  let chan = open_in f in
  let buf = Buffer.create 1024 in
  let rec read_file_into_buffer_aux bs chan buf =
    let n = input chan bs 0 512 in
    if n = 0 then ()
    else begin
	Buffer.add_bytes buf bs;
	read_file_into_buffer_aux bs chan buf;
      end in
  read_file_into_buffer_aux bs chan buf;
  close_in chan;
  buf

let init_scanner_from_file file = 
  {buf = read_file_into_buffer file; offset = 0; next_offset = 0; ch = ' '}
    
let init_scanner_from_string str = 
  let buf = Buffer.create (String.length str) in
  let s = {buf = buf; offset = 0; next_offset = 0; ch = ' '} in
  Buffer.add_string s.buf str; s
				 
let next s = 
  if s.next_offset >= Buffer.length s.buf then 
    (s.ch <- '\000'; 
     s.offset <- s.next_offset)
  else 
    (s.ch <- Buffer.nth s.buf s.next_offset;
     s.offset <- s.next_offset;
     s.next_offset <- s.next_offset + 1)
  

let peek s = s.ch
let peeknext s = next s; s.ch
let word s start = Buffer.sub s.buf start (s.offset - start)

(* character predicates *)
let is_blank ch = ch = ' ' || ch = '\t'
let is_digit ch = let code = Char.code(ch) in code >= Char.code('0') && code <= Char.code('9')
let is_first_digit ch = is_digit ch || ch = '_' || ch = '.'
let is_alpha ch = let code = Char.code ch in (code >= Char.code('a') && code <= Char.code('z')) ||
					    (code >= Char.code('A') && code <= Char.code('Z'))
let is_alphanum ch = is_alpha ch || is_digit ch || ch = '_'
let is_newline ch = ch = '\n'
let is_inflection ch = ch = ':' || ch = '.'
let is_wordchar ch = is_alphanum ch || is_inflection ch
let is_wordsep ch = is_blank ch || is_newline ch || not (is_inflection ch || is_alphanum ch)
let is_false ch = false
		 
(* scanning helpers *)
let rec consume pred s = if pred s.ch then (next s; consume pred s) else ()
	       
let consume_blanks s = consume is_blank s
let consume_alphanums s = consume is_alphanum s
let consume_inflections s = consume is_inflection s
let consume_word s = consume is_wordchar s
let consume_digits s = consume is_digit s

(* scanning sub-functions *)
let scan_inflections s = 
  let start = s.offset in
  consume_inflections s;
  Buffer.sub s.buf start (s.offset - start)

(* Handles names and primitives that start with a letter *)	     
let scan_alpha s start = 
  consume_alphanums s;
  let i = scan_inflections s in
  if i = "" then Name (word s start)
  else let w =  word s start in
       match w with
       | _ -> Illegal

let rec scan_choices s tok choices =
  let ch = peek s in
  match choices with
    [] -> tok
  | (pred, scan_func)::chcs -> if pred ch then scan_func s
			       else scan_choices s tok chcs
let scan_number s start =
  consume_digits s;
  Noun (Numeric, OpenRank, OpenShape)

let scan_newline s = next s; EOS

let scan s =
  consume_blanks s;
  let ch = peek s in
  let start = s.offset in
  if is_alpha ch then 
    let t = scan_alpha s start in
    {tok=t; start=start; stop=s.offset}
  else if is_first_digit ch then 
    let t = scan_number s start in
    {tok=t; start=start; stop=s.offset}
  else if is_newline ch then 
    let t = scan_newline s in
    {tok=t; start=start; stop=s.offset}
  else begin
      next s;
      consume_inflections s;
      let t = match word s start with
	| "+"  -> Verb (Prim Plus)
	| "+." -> Verb (Prim Plus1)
	| "+:" -> Verb (Prim Plus2)
	| "-"  -> Verb (Prim Minus)
	| "-." -> Verb (Prim Minus1)
	| "-:" -> Verb (Prim Minus2)
	| "/"  -> Adv (Adverb [FSlash])
	| "\000" -> EOS
	| _ -> Illegal in
      {tok=t; start=start; stop=s.offset}
    end

  
