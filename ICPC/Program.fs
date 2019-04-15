module ICPC
open System

type Token =
    |Word of string
    |Comma
    |Space
    |Fullstop

let commaSprinkler input =
   let wordslist = input.Split ' '
   let rec goThroughList s count newlist =
       let s = Seq.ToList wordslist.[0]
       let srev = List.rev s
       let shead = List.head srev
       match shead with
       |',' -> Comma
       |'.' ->Fullstop
       |_ -> 
           match shead = Comma with
           |true -> s.tail  

     


     
    // let listString = List.ofSeq (input)


let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
         