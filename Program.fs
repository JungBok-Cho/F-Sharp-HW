(* CPSC 3400-01
   JungBok Cho
   F# Assignment 2

   Originality of Code: 
   I composed this code I wrote based on my understanding of how the features of 
   the language I am using can be used to implement the algorithm I have chosen 
   to solve the problem I am addressing. *)

open System

/// findNum function will retrieve the value associated 
/// with that letter and push it on an operand stack
let rec findNum element vars = 
    match vars with
    | [] -> 0
    | (a, b) :: tl -> if char a = element then b
                      else findNum element tl

/// checkExpr function will update the stack based on the requested operator
let checkExpr element vars (stack : int list) = 
    match element with
    | '+' -> (stack.Tail.Head + stack.Head) :: stack.Tail.Tail  /// Plus two values
    | '-' -> (stack.Tail.Head - stack.Head) :: stack.Tail.Tail  /// Subtract two values
    | '*' -> (stack.Tail.Head * stack.Head) :: stack.Tail.Tail  /// Multiply two values
    | '/' -> (stack.Tail.Head / stack.Head) :: stack.Tail.Tail  /// Divide two values
    | '$' -> stack.Tail.Head :: stack.Head :: stack.Tail.Tail   /// Swap two values
    | ' ' -> stack                                              /// Skip
    | _   -> (findNum element vars) :: stack                    /// Find value and add to stack

/// Postfix Expression Evaluation
let eval vars expr = 
    let rec innerEval vars (stack : int list) expr = 
        match expr with
        | [] -> stack.Head 
        /// If hd == @, then update vars. 
        /// Otherwise, call checkExpr function to update stack.
        | hd :: (tl : char list) -> if hd = '@' then
                                       let newVars = (string tl.Head, stack.Head) :: vars 
                                       innerEval newVars stack tl.Tail
                                    else 
                                       let newStack = checkExpr hd vars stack
                                       innerEval vars newStack tl
    innerEval vars [] (Seq.toList expr) 


/// Provided Codes for Testing by Professor
/// The answer should be [7; 2; -2; 49; 51; 0; 64; 5] 
let testEval = eval [("a",5);("b",2);("c",9)] 
let exprList = ["ab+"; "cab+-"; "cab+$-"; "ab+cb-*"; "ab+cb-* @d bd+"; 
                "ab-ab*ab+--"; "bbb @q bqbq*****"; "ca- @b bc$-"]
let resultList = List.map testEval exprList
