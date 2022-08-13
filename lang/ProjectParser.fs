module ProjectParser

open Parser
open System

type PixelPunk =
| Color of string
| Pixel of int * int * PixelPunk // Treated as int * int * Color
| Attribute of PixelPunk list // Treated as Pixel list

let pPixelPunk, pPixelPunkImpl = recparser()

// all valid values in a hex color code
let hexDigits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F']
// helper function for color parser that checks if a character is a valid hex digit
let isValidHexDigit d =
    List.exists (fun digit -> digit = d) hexDigits

// helper function for color parser that checks if a string is a valid hex color code
let hexCheck = pmany1 (psat (fun d -> isValidHexDigit d))
let pColor : Parser<PixelPunk> = pright (pstr "color=#") (hexCheck |>> stringify |>> (fun c -> Color(c)))

// checks if row/col index is valid
// must be an int from 0 to 24 inclusive
let isValidIndex i =
    let num = i |> string |> int
    if num <= 24 && num >= 0
    then true
    else
    false

// helper function for pixel parser that checks if row is valid
let row = pbetween (pstr "row=") (pchar ',') (pmany1 pdigit)
// helper function for pixel parser that checks if column is valid
let col = pbetween (pstr "col=") (pchar ',') (pmany1 pdigit)
// helper function for pixel parser that combines row and column
let pixelIndex = pseq row col (fun (x, y) -> (x |> stringify |> int, y |> stringify |> int))
// helper function for pixel parser that feeds "inner" contents (row, col, color) to pixel constructor
let innerPixel = pseq pixelIndex pColor (fun ((i, j), c) -> Pixel(i, j, c)) 
// syntax for creating a pixel: Pixel(row=<int>, col=<int>, color=<string>)
let pPixel : Parser<PixelPunk> = pbetween (pstr "Pixel(") (pchar ')') innerPixel
// helper function for attribute helper tha(t checks if we have a valid pixel list
let attributeHelper = pmany1 (pleft pPixel (pmany0 pnl)) |>> (fun p -> Attribute p)

// syntax for attribute: Atrribute={<Pixel_List_separated_by_new_lines>}
let pAttribute : Parser<PixelPunk> = pbetween (pstr "Attribute={") (pchar '}') attributeHelper

pPixelPunkImpl := pColor <|> pPixel <|> pAttribute

let grammar : Parser<PixelPunk> = pleft pPixelPunk peof

let parse(program: string) : PixelPunk option =
    match grammar (prepare program) with
    | Success(res, _) -> Some(res)
    | Failure(_, _) -> None

