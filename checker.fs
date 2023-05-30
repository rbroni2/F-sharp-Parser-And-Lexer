//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
//
// Modified by:
//   << Robert Broniarczyk >>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //
  let rec private contains token L = // checking L contains a token
    match L with
    |(hd1,hd2)::tl when hd1 = (string(token).Substring(11)) -> (hd1,hd2)
    |(hd1,hd2)::tl -> contains token tl
    | [] -> failwith ("variable '" + (string(token).Substring(11)) + "' undefined")
  let rec private idCheck t1 L = // obtain type of identifier using tuple list L
   match L with
   |(hd1,hd2)::tl when (string(t1).Substring(11)) = hd1 -> hd2
   |(hd1,hd2)::tl -> idCheck t1 tl 
   |[] -> ""

  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation:
    //
    // NOTE: identifier, int_literal and str_literal
    // are special cases because they are followed by
    // the name or literal value. In these cases exact
    // matching will not work, so we match the start 
    // of the token in these cases.
    //
    let next_token = List.head tokens

    if expected_token = "identifier" && next_token.StartsWith("identifier") then
      //
      // next_token starts with identifier, so we have a match:
      //
      List.tail tokens
    elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
      //
      // next_token starts with int_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
      //
      // next_token starts with str_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
      //
      // next_token starts with str_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  //
  // <expr-value> -> identifier
  //               | int_literal
  //               | str_literal
  //               | true
  //               | false
  //
  let rec private expr_value tokens L =
    let next_token = List.head tokens
    //
    if next_token = "false" then
      let T2 = matchToken "false" tokens
      (T2,"bool")
    elif next_token = "true" then
      let T2 = matchToken "true" tokens
      (T2,"bool")
    //
    // the others are trickier since we have to look 
    // at the start of the string for a match:
    //
    elif next_token.StartsWith("identifier") then
      let test2 = contains next_token L
      let T2 = matchToken "identifier" tokens
      let tyty = idCheck next_token L
      (T2,tyty)
    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      (T2,"int")
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      (T2,"str")
    elif next_token.StartsWith("real_literal") then
      let T2 = matchToken "real_literal" tokens
      (T2,"real")
    else
      failwith ("expecting identifier or literal, but found " + next_token)


  //
  // <expr-op> -> +
  //            | -
  //            | *
  //            | /
  //            | ^
  //            | <
  //            | <=
  //            | >
  //            | >=
  //            | ==
  //            | !=
  //
  let rec private expr_op tokens = 
    let next_token = List.head tokens
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T2 = matchToken next_token tokens
      T2
    else
      // error
      failwith ("expecting expression operator, but found " + next_token)


  //
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens L = 
    //
    // first we have to match expr-value, since both
    // rules start with this:
    //
    let T1 = List.head tokens
    let (T2,MaiTai) = expr_value tokens L
    //
    // now let's see if there's more to the expression:
    //
    let next_token = List.head T2
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T3 = expr_op T2
      let finalT = List.head T3
      let (T4,arghh) = expr_value T3 L
      if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  then
       if (MaiTai = "int" || MaiTai = "real") && 
         (arghh = "int" || arghh = "real") then // check if types are int/real in math
        if arghh <> MaiTai then // checking if matching type
         failwith ("operator " + next_token + " must involve 'int' or 'real'")
       else
         failwith ("operator " + next_token + " must involve 'int' or 'real'")

       (T4,MaiTai)
      elif next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "!=" then
       if arghh <> MaiTai then//check if matching type in bool statement
         failwith ("type mismatch '" + MaiTai + "' " + next_token + " '" + arghh + "'")
       (T4, "bool")//returning type bool
      elif  next_token = "==" then
       if arghh = "real" || MaiTai = "real" then // warning if real used with ==
        printfn "warning: comparing real numbers with == may never be true"
       if arghh <> MaiTai then // type mismatch in == 
         failwith ("type mismatch '" + MaiTai + "' " + next_token + " '" + arghh + "'")
       (T4, "bool") // now is bool
      else
       (T4,MaiTai)
    else
      // just expr_value, that's it
      (T2,MaiTai)


  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2


  // let rec private redefCheck S L = // checking if a var is re defined
  //   match L with
  //   |(hd1, hd2)::tl when hd1 = S -> failwith("redefinition of variable '" + S + "'")
  //   | _::tl -> redefCheck S tl
  //   | [] -> []
  //
  // <vardecl> -> int identifier ;
  //
  let rec private vardecl tokens L= 
    let T2 = matchToken "int" tokens
    let T3 = matchToken "identifier" T2
    let T4 = matchToken ";" T3
    // let Re = redefCheck ((List.head T2).Substring(11)) L 
    // let L2 = ((List.head T2).Substring(11), "int")::L
    (T4, L)

  let rec private vardecl2 tokens L = 
    let T2 = matchToken "real" tokens
    let T3 = matchToken "identifier" T2
    let T4 = matchToken ";" T3
    // let Re = redefCheck ((List.head T2).Substring(11)) L
    // let L2 = ((List.head T2).Substring(11), "real")::L
    (T4, L)
    //let L2 = (remove)


  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens L = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let (ts1,ts2) = contains (List.head T3) L 
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens L = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let (T2,big_sad) = expr_value tokens L
      T2


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens L = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3 L
    let T5 = matchToken ";" T4
    T5

  
  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens L= 
    let (hd1,hd2) = contains (List.head tokens) L
    let oldType = idCheck (List.head tokens) L
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let (T4,tyNode) = expr T3 L
    
    if oldType <> tyNode then // checking  assign type match
      if oldType <> "real" || tyNode <> "int" then // exception with real given int to avoid false error
       failwith ("cannot assign '" + tyNode + "' to variable of type '" + oldType + "'")
    let T5 = matchToken ";" T4
    T5


  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens L = 
    let next_token = List.head tokens
    //
    // use the next token to determine which rule
    // to call; if none match then it's a syntax
    // error:
    //
    if next_token = ";" then
      let T2 = empty tokens
      (T2, L)
    elif next_token = "int" then
      let (T2,L2) = vardecl tokens L
      (T2, L2)
    elif next_token = "real" then
      let (T2, L2) = vardecl2 tokens L
      (T2, L2)
    elif next_token = "cin" then
      let T2 = input tokens L
      (T2, L)
    elif next_token = "cout" then
      let T2 = output tokens L
      (T2, L)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens L
      (T2, L)
    elif next_token = "if" then
      let (T2,L2) = ifstmt tokens L
      (T2, L2)
    else
      failwith ("expecting statement, but found " + next_token)
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens L = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 L
    let T5 = matchToken ")" T4
    let (T6,L) = then_part T5 L
    let (T7,L) = else_part T6 L
    (T7, L)
  //
  // <condition> -> <expr>
  //
  and private condition tokens L= 
    let (T2,myBol) = expr tokens L
    if myBol <> "bool" then // check if bool type
      failwith ("if condition must be 'bool', but found '" + myBol + "'")
    T2
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens L= 
    let (T2, L) = stmt tokens L
    (T2,L)
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens L = 
    let next_token = List.head tokens
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3, L) = stmt T2 L
      (T3, L)
    else
      // EMPTY, do nothing but return tokens back
      (tokens, L)


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens L = 
    //
    // if the next token denotes the start of a stmt 
    // then process stmt and morestmts, otherwise apply
    // EMPTY
    //
    let next_token = List.head tokens
    //
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "real" ||
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" then
      let (T2, L2) = stmt tokens L
      let (T3,L3) = morestmts T2 L2
      (T3, L3)
    else 
      // EMPTY => do nothing, just return tokens back
      (tokens, L)


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens L = 
    let (T2,L2) = stmt tokens L
    let (T3,L3) = morestmts T2 L2
    (T3, L3)

  let private simpleC tokens symboltable = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, L2) = stmts    T6 symboltable
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8  // $ => EOF, there should be no more tokens
    T9


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message

