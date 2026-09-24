


// exact length type
type Exact<'H, 'T> = 'H * 'T

let exact4 = (1, (2, (3, (4, ()))))

let phead1 = function | h, tail -> sprintf "%A" h
let phead2 = function | h, tail -> sprintf "%A" h + phead1 tail
let phead3 = function | h, tail -> sprintf "%A" h + phead2 tail
let phead4 = function | h, tail -> sprintf "%A" h + phead3 tail

printfn "%s" (phead4 exact4)

type myList<'T> = 
    | Nil
    | Cons of 'T * myList<'T>

// minimum list length type

type Minlength4<'T> = Exact<'T, Exact<'T, Exact<'T, Exact<'T, myList<'T>>>>>

let minlength4 = (1, (2, (3, (4, Cons(5, Nil)))))

let rec printList = function
    | Nil -> ""
    | Cons(h, t) -> sprintf "%A" h + printList t

let printm1 = function| h, tail -> sprintf "%A" h + printList tail
let printm2 = function| h, tail -> sprintf "%A" h + printm1 tail
let printm3 = function| h, tail -> sprintf "%A" h + printm2 tail
let printm4 = function| h, tail -> sprintf "%A" h + printm3 tail

printfn "%s" (printm4 minlength4)

// maxitems
type Maxitems4<'T> = 