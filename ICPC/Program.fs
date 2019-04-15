    
module ICPC
open System
open System.Net

type Token =
    |Word of string
    |Comma
    |Space
    |Fullstop

let splitLine = (fun (line : string) -> Seq.toList (line.Split ' ')) //taken from https://www.dotnetperls.com/split-fs

//code taken from http://www.fssnip.net/5u/title/String-explode-and-implode
let chartostring (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()
 
 //add comma to preceding word
let rec addCommaPre count oldlist newlist w = 
   match count < List.length oldlist with
   |true -> 
            let result = oldlist.[count]
            match Word w = oldlist.[count] with
            |true -> match not(count=0) && not(oldlist.[count-1]=Comma) && not(oldlist.[count-1]=Fullstop) with
                     |true -> let newlist = Comma::newlist
                              let newlist = Word w::newlist
                              addCommaPre (count+1) oldlist newlist w
                     |false -> let newlist = Word w::newlist
                               addCommaPre (count+1) oldlist newlist w
            |false -> let newlist = oldlist.[count]::newlist 
                      addCommaPre (count+1) oldlist newlist w
   |false -> newlist

//add comma to succeeding word
let rec addCommaPost count oldlist newlist w =
   match count < List.length oldlist with
   |true -> match Word w = oldlist.[count] with
            |true -> 
                     match not(oldlist.[count+1]=Fullstop) && not(oldlist.[count+1] = Comma) with
                     |true -> let newlist = Word w::newlist
                              let newlist = Comma::newlist
                              addCommaPost (count+1) oldlist newlist w  
                     |false ->let newlist = Word w::newlist
                              addCommaPost (count+1) oldlist newlist w
            |false ->let newlist = oldlist.[count]::newlist
                     addCommaPost (count+1) oldlist newlist w
   |false -> newlist

//check if string is valid for Problem B
let rec checkValidString clist count =
 match clist with 
 |[] -> []
 |_ ->
   match count < List.length clist with
   |true -> match System.Char.IsLetter(clist.[count]) || (clist.[count]).Equals(' ') || (clist.[count]).Equals(',') || (clist.[count]).Equals('.') with
               |true -> match clist.[count] with
                        |',' -> match count+1 < List.length clist with
                                |true -> match clist.[count+1] with 
                                         |' '-> checkValidString clist (count+1)
                                         |',' ->[]
                                         |'.' ->[]
                                         |_ -> []
                                |false -> checkValidString clist (count+1)
                        |'.' -> match count+1 < List.length clist with
                                |true-> match clist.[count+1] with 
                                        |' ' -> checkValidString clist (count+1)
                                        |'.' -> []
                                        |',' -> []
                                        |_ -> checkValidString clist (count+1)
                                |false -> checkValidString clist (count+1)
                        |' ' -> match count+1 < List.length clist with
                                |true -> match clist.[count+1] with
                                         |' ' -> []
                                         |',' -> []
                                         |'.' -> []
                                         |_ -> checkValidString clist (count+1)
                                |false -> checkValidString clist (count+1)
                        |_ -> match count+1 < List.length clist with
                              |true -> match clist.[count+1] with
                                       |_ -> checkValidString clist (count+1)
                              |false -> checkValidString clist (count+1)
               |false -> []
   |false -> clist
    

let commaSprinkler input =
   let stringlist = splitLine input
   let stringlist = match stringlist.Head with
                    |"," -> []
                    |" " ->[]
                    |"" ->[]
                    |"." ->[]
                    |_ -> stringlist
   let charlist = List.ofSeq input
   let answer = checkValidString charlist 0 
   let stringlist = 
      match answer with
      |[] -> []
      |_ -> stringlist
   let tlist = []

   //function to make tokenised list
   let rec goThroughList slist count tlist =
      match count < List.length slist with
        |true -> let string = slist.[count]
                 let charlist = List.ofSeq string
                 let charlistr = List.rev charlist
                 let tlist = 
                  match charlistr with 
                  |[] -> []
                  |head::tail -> match head with 
                                 |',' -> 
                                        match tail with 
                                        |_ -> let wordstring = chartostring (List.rev tail)
                                              let result = Word wordstring
                                              let tlist = result::tlist
                                              Comma::tlist
                                 |'.' -> 
                                        match tail with 
                                        |_ -> let wordstring = chartostring (List.rev tail)
                                              let result = Word wordstring
                                              let tlist = result::tlist
                                              Fullstop::tlist
                                 |_ -> let wordstring = chartostring (charlist)
                                       let result = Word wordstring
                                       result::tlist
                 goThroughList slist (count+1) tlist
        |false -> tlist
   let tlist = goThroughList stringlist 0 tlist
   let tokenlist = List.rev tlist

   //creating a newtokenlist by finding words that pre or post a comma and adding commas to the newtokenlist
   let rec findCommaWords tlist count newlist =
        match count < List.length newlist with
        |true ->
            let index1 = count-1
            let index2 = count+1
            let answer = newlist.[count]
            match answer with 
              |Comma -> match newlist.[index1] with
                        |Word a -> let result = a
                                   let newlist = addCommaPost 0 newlist [] result
                                   let newlist = List.rev newlist
                                   match newlist.[index2] with
                                   |Word a -> let result = a
                                              let newlist = addCommaPre 0 newlist [] result
                                              let newlist = List.rev newlist
                                              findCommaWords tlist (count+1) newlist
                        |_ -> match newlist.[index2] with
                              |Word a -> let result = a
                                         let newlist = addCommaPre 0 newlist [] result
                                         let newlist = List.rev newlist
                                         findCommaWords tlist (count+1) newlist
              |_ -> //let newlist = answer::newlist
                    findCommaWords tlist (count+1) newlist
        |false -> newlist
   let newtokenlist = findCommaWords tokenlist 0 tokenlist
   
 
   //recompose list from tokens to strings
   let newslist =[]
   let listlengthcheck = (List.length newtokenlist)-1
   let rec recompose oldtlist count newslist =
     match count < List.length oldtlist with
     |true-> match oldtlist.[count] with
             |Comma -> let newslist = ","::newslist
                       //let newslist = " "::newslist
                       recompose oldtlist (count+1) newslist
             |Fullstop -> let newslist =  "."::newslist
                          match count < listlengthcheck with
                          |true -> //let newslist = " "::newslist
                                   recompose oldtlist (count+1) newslist
                          |false -> recompose oldtlist (count+1) newslist
             |Word a -> match (count=0) with 
                        |true -> let newslist = a::newslist
                                 recompose oldtlist (count+1) newslist
                        |false -> match not(count=0) && oldtlist.[count-1]=Comma with
                                  |true ->let newslist = " "::newslist
                                          let newslist = a::newslist
                                          recompose oldtlist (count+1) newslist
                                  |false ->let newslist = " "::newslist
                                           let newslist = a::newslist
                                           recompose oldtlist (count+1) newslist
     |false -> newslist
   let newslist = recompose newtokenlist 0 [] 

   let newlist = List.rev newslist
   //Options for error cases
   match newlist with
   |[] -> None 
   |_ -> match List.length newlist with
         |1 -> None
         |_ -> match newlist.Head with
               |"," ->None
               |"." ->None
               |_ -> 
                     match List.last newlist with
                     |" " -> None
                     |_ -> let newstring = newlist |> List.fold (+) ""
                           //let charlist = List.ofSeq newstring
 
                           match (String.exists (fun c -> System.Char.IsUpper(c)) newstring) with
                                 |true -> None
                                 |false -> Some newstring

//////////////////////////////////////////RIVERS PROBLEM///////////////////////////////////////////////////////

(*let rivers input =
    failwith ""
    let stringlist = splitLine input
    let rec listlengthlist slist count llist =
        match count < List.length slist with
        |true -> let stringlength = String.length slist.[count]
                 match stringlength <= 80 with
                   |true -> String.length slist.[count]::llist
                            (listlengthlist slist (count+1) llist)
                   |false -> [] 
        |false -> llist
    let lengthlist = listlengthlist stringlist 0 []
    let lengthlistmax = List.max lengthlist

    match lengthlist with
    |[] -> None
    |_ -> Some lengthlist
    *)
   (* let rec wrapText llist count count2 countall list1 list2  = 
          match count < List.length llist with
          |true -> let countall = llist.[count]+1+countall
                   match countall > lengthlistmax with
                   |true -> let index = count-1
                            let result =
                              match count > 0 with
                              |true -> list1.[count-1]+1
                              |false -> 0
                            result::list1
                            llist.[count]::list2
                            let count2 = list1.[count]
                            let countall = list2.[count]
                            wrapText llist (count+1) count2 countall list1 list2
                   |false -> let count2 = list1.[count]
                             let countall = list2.[count]
                             wrapText llist (count+1) count2 countall list1 list2
          |false -> list1, list2
    wrapText lengthlist 0 0 0 [] []
    *)
    
let rivers input = 
 let stringlist = splitLine input

 let rec checkValidString clist count =
  match clist with 
  |[] -> []
  |_ ->
 
             match count < List.length clist with
             |true -> match System.Char.IsLetter(clist.[count]) || (clist.[count]).Equals(' ')  with
               |true -> match clist.[count] with
                        |' ' -> match count+1 < List.length clist with
                                |true -> match clist.[count+1] with
                                         |' ' -> []
                                         |_ -> checkValidString clist (count+1)
                                |false -> checkValidString clist (count+1)
                        |_ -> match count+1 < List.length clist with
                              |true -> match clist.[count+1] with
                                       |_ -> checkValidString clist (count+1)
                              |false -> checkValidString clist (count+1)
               |false -> []
             |false -> clist
 let charlist = List.ofSeq input

 let answer = checkValidString charlist 0 

 match answer with 
  |[] -> None
  |_  -> Some answer



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code