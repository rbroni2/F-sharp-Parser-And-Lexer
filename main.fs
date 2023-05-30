//
// F# program to input a string and print out information
// about the # of vowels and digraphs in that string.
//
// Name: Robert Broniarczyk
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode (S:string) = 
  List.ofArray (S.ToCharArray())

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e'] => "the"
//
let implode (L:char list) = 
  new string(List.toArray L)


let rec length L = //gets length from based on in class
  match L with
  | [] -> 0
  | head::[] -> 1
  | head::tail -> 1 + length(tail)

let numVowels L = //get num of each vowel using filter then sum
  let x = 'a'
  let subset = List.filter (fun e -> e = x) L
  let x2 = 'e'
  let subset2 = List.filter (fun e -> e = x2) L
  let x3 = 'i'
  let subset3 = List.filter (fun e -> e = x3) L
  let x4 = 'o'
  let subset4 = List.filter (fun e -> e = x4) L
  let x5 = 'u'
  let subset5 = List.filter (fun e -> e = x5) L
  let final =  length subset + length subset2 +  length subset3 + length subset4 + length subset5
  final
let vowel x L = //get number of specified vowel x
  let vowela = List.filter (fun e -> e = x) L
  let totala = length vowela
  totala
  
let rec digraph c1 c2 L = //check for digraph of c1 c2 recursively
  match L with
  | [] -> 0
  | head::[] -> 0
  | head::tail when head = c1 && List.head tail = c2 -> 1 + digraph c1 c2 tail
  | _::tail -> 0 + digraph c1 c2 tail 

[<EntryPoint>]
let main argv =
  printfn "Starting"
  printfn ""

  //
  // input string, output length and # of vowels:
  //
  printf("input> ")
  let input = System.Console.ReadLine()

  let L = explode input
  printfn "exploded: %A" L

  let len = length L // length and number of each vowel outputed and computed
  printfn "length: %A" len
  let num = numVowels L
  printfn "vowels: %A" num
  let x = 'a'
  let totala = vowel x L
  printfn "'a': %A" totala
  let x2 = 'e'
  let totale = vowel x2 L
  printfn "'e': %A" totale
  let x3 = 'i'
  let totali = vowel x3 L
  printfn "'i': %A" totali
  let x4 = 'o'
  let totalo = vowel x4 L
  printfn "'o': %A" totalo
  let x5 = 'u'
  let totalu = vowel x5 L
  printfn "'u': %A" totalu
  let test = digraph 'c' 'h' L//all individual digraph outputs
  let annoying = digraph 'a' 'i' L
  let things = digraph 'e' 'a' L
  let ie = digraph 'i' 'e' L
  let ou = digraph 'o' 'u' L
  let ph = digraph 'p' 'h' L
  let sh = digraph 's' 'h' L
  let th = digraph 't' 'h' L
  let wh = digraph 'w' 'h' L
  let digraphs = [digraph 'c' 'h' L; digraph 't' 'h' L; digraph 'a' 'i' L; digraph 'e' 'a' L; digraph 'p' 'h' L; digraph 's' 'h' L; digraph 'w' 'h' L; digraph 'i' 'e' L; digraph 'o' 'u' L]//list of digraph outputs
  let totaldi = List.sum digraphs
  
  printfn "digraphs: %A" totaldi//output digraph data
  printfn "'a','i': %A" annoying
  printfn "'c','h': %A" test
  printfn "'e','a': %A" things
  printfn "'i','e': %A" ie
  printfn "'o','u': %A" ou
  printfn "'p','h': %A" ph
  printfn "'s','h': %A" sh
  printfn "'t','h': %A" th
  printfn "'w','h': %A" wh
  
   

  //
  // TODO: print count of each vowel:
  //
  
  
  //
  // TODO: print number of digraphs, count of each:
  //


  //
  // done: implode list, print, and return
  //
  let S = implode L
  printfn "imploded: %A" S

  printfn ""
  printfn "Done"
  0  // return 0 => success, much like C++