


type datatype<'a> = 
    | Nil 
    | Cons of 'a * datatype<'a>
type recType<'a> =
    | SingleTon of 'a * Nil
