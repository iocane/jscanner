type nountype = Numeric | Char
type nounrank = OpenRank | NounRank of int
type nounshape = OpenShape | NounShape of int list

type advsym =
  | Tilde
  | RBrace
  | BSlash | BSlash1
  | DQuote
  | FSlash | FSlash1

type conjsym = 
  | At     | At1     | At2
                     | Expo2
  | Amp    | Amp1    | Amp2   | Amp12
  | Colon  | Colon1  | Colon2
  | Dot    | Dot1    | Dot2

type verbsym = 
           | Tilde1  | Tilde2
  | Bang   | Bang1   | Bang2
  | Hash   | Hash1   | Hash2
  | Dollar | Dollar1 | Dollar2
  | Divide | Divide1 | Divide2
  | Expo   | Expo1
  | Times  | Times1  | Times2
  | Minus  | Minus1  | Minus2
  | Plus   | Plus1   | Plus2
  | Equal  
  | LBrack           | LBrack2
  | LBrace | LBrace1 | LBrace2 | LBrace22
  | RBrack
           | RBrace1 | RBrace2
                     | BSlash2
  | Pipe   | Pipe1   | Pipe2
  | Semi   | Semi1   | Semi2
           | DQuote1 | DQuote2
  | Comma  | Comma1  | Comma2
  | Less   | Less1   | Less2
  | Great  | Great1  | Great2
                     | FSlash2
  | QMark  | QMark1

type adverb = Adverb of primadv list
type conjunction = Conjunction of primconj			       

type copulasym = Local | Global

type verb =
  | Prim of primverb
  | Derived of derived
 and derived = 
  | Hook of verb * verb
  | Fork of verb * verb * verb
  | Adverbial of adverb list * verb
  | Conjunctional of conjunction * verb * verb
					    
type token = 
  | TIllegal
  | TName of string
  | TNoun of nountype * nounrank * nounshape
  | TVerb of verbsym
  | TAdv of advsym
  | TConj of conjsym
  | TCopula of copulasym
  | TEOS
  | TEOF

type node = {tok:token; start:int; stop:int}
