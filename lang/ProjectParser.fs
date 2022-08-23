module ProjectParser

open Parser
open System

// final PixelPunk type
type PixelPunk =
| Color of string
| Pixel of int * int * PixelPunk // Treated as int * int * Color
| Attribute of PixelPunk list // Treated as Pixel list
| Canvas of PixelPunk // treat as Color
| Sequence of PixelPunk list // stores Attributes, Assignments, and Variables
| Variable of string
| Assignment of PixelPunk * PixelPunk // Variable * PixelPunk; PixelPunk can be anything but Color, Variable and Assignment

let pPixelPunk, pPixelPunkImpl = recparser()

// From Dan's example
(* pcomment
 *   # Comments start with a hashtag but can't contain
 *   # one (or a newline).
 *)
let pnotcom = psat (fun c -> c <> '#' && c <> '\n')
let pcomment =
    pbetween (pchar '#') peof (pmany1 pnotcom)
    <|> pbetween (pchar '#') pws1 (pmany1 pnotcom)
    |>> (fun _ -> true)
    <!> "pcomment"

/// From Dan's example
(* my_ws
 *   Let's consider any non-newline whitespace or
 *   a comment to be whitespace
 *)
let my_ws = pcomment <|> (pwsNoNL0 |>> (fun _ -> true))

// From Dan's example
(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween my_ws my_ws p

// all valid values in a hex color code
let hexDigits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F']
// helper function for color parser that checks if a character is a valid hex digit
let isValidHexDigit d =
    List.exists (fun digit -> digit = d) hexDigits
    
// helper function for color parser that checks if a string is a valid hex color code
let hexCheck = pright (pchar '#') (pmany1 (psat isValidHexDigit))
// syntax for color: color=<hex_color_code>
let pColor : Parser<PixelPunk> = pright (pstr "color=") (hexCheck |>> stringify |>> (fun c -> if c.Length = 6 then Color(c) else failwith("Invalid Hex Color Code.\n")))

// checks if row/col index is valid
// must be an int from 0 to 24 inclusive
let isValidIndex (x, y) =
    let row = x |> stringify |> int
    let col = y |> stringify |> int
    if row <= 24 && row >= 1 && col <= 24 && col >= 1
    then (row, col)
    else
    failwith("Invalid Index.\n")

// helper function for pixel parser that checks if row is valid
let row = pbetween (pright (pad (pstr "row")) (pad (pchar '='))) (pad (pchar ',')) (pmany1 pdigit)
// helper function for pixel parser that checks if column is valid
let col = pbetween (pright (pad (pstr "col")) (pad (pchar '='))) (pad (pchar ',')) (pmany1 pdigit)
// helper function for pixel parser that combines row and column
let pixelIndex = pseq row col isValidIndex
// helper function for pixel parser that feeds "inner" contents (row, col, color) to pixel constructor
let innerPixel = pseq pixelIndex pColor (fun ((i, j), c) -> Pixel(i, j, c)) 
// syntax for creating a pixel: Pixel(row=<int>, col=<int>, color=<string>)
let pPixel : Parser<PixelPunk> = pbetween (pstr "Pixel(") (pchar ')') innerPixel

let pCanvas : Parser<PixelPunk> = pbetween (pleft (pstr "Canvas{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> (fun c -> Canvas(c))
// helper function for attribute helper tha(t checks if we have a valid pixel list
let attributeHelper = pmany1 (pleft pPixel (pmany0 pnl)) |>> (fun p -> Attribute p)

// syntax for attribute: Attribute={<Pixel_List_separated_by_new_lines>}
let pAttribute : Parser<PixelPunk> = pbetween (pleft (pstr "Attribute{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) attributeHelper

(*
    The following section contains all of the hardcoded attributes we provide for the user
*)

// helper function that contains the pixel list for a head
let pHeadHelper = (fun c -> Attribute([Pixel(6,9,c);Pixel(6,10,c);Pixel(6,11,c);Pixel(6,12,c);Pixel(6,13,c);Pixel(6,14,c);Pixel(6,15,c);
        Pixel(7,8,c);Pixel(7,9,c);Pixel(7,10,c);Pixel(7,11,c);Pixel(7,12,c);Pixel(7,13,c);Pixel(7,14,c);Pixel(7,15,c);Pixel(7,16,c);Pixel(8,7,c);
        Pixel(8,8,c);Pixel(8,9,c);Pixel(8,10,c);Pixel(8,11,c);Pixel(8,12,c);Pixel(8,13,c);Pixel(8,14,c);Pixel(8,15,c);Pixel(8,16,c);Pixel(8,17,c);
        Pixel(9,7,c);Pixel(9,8,c);Pixel(9,9,c);Pixel(9,10,c);Pixel(9,11,c);Pixel(9,12,c);Pixel(9,13,c);Pixel(9,14,c);Pixel(9,15,c);Pixel(9,16,c);
        Pixel(9,17,c);Pixel(10,7,c);Pixel(10,8,c);Pixel(10,9,c);Pixel(10,10,c);Pixel(10,11,c);Pixel(10,12,c);Pixel(10,13,c);Pixel(10,14,c);Pixel(10,15,c);
        Pixel(10,16,c);Pixel(10,17,c);Pixel(11,7,c);Pixel(11,8,c);Pixel(11,9,c);Pixel(11,10,c);Pixel(11,11,c);Pixel(11,12,c);Pixel(11,13,c);Pixel(11,14,c);
        Pixel(11,15,c);Pixel(11,16,c);Pixel(11,17,c);Pixel(12,7,c);Pixel(12,8,c);Pixel(12,9,c);Pixel(12,10,c);Pixel(12,11,c);Pixel(12,12,c);Pixel(12,13,c);
        Pixel(12,14,c);Pixel(12,15,c);Pixel(12,16,c);Pixel(12,17,c);Pixel(13,6,c);Pixel(13,7,c);Pixel(13,8,c);Pixel(13,9,c);Pixel(13,10,c);Pixel(13,11,c);
        Pixel(13,12,c);Pixel(13,13,c);Pixel(13,14,c);Pixel(13,15,c);Pixel(13,16,c);Pixel(13,17,c);Pixel(14,6,c);Pixel(14,7,c);Pixel(14,8,c);Pixel(14,9,c);
        Pixel(14,10,c);Pixel(14,11,c);Pixel(14,12,c);Pixel(14,13,c);Pixel(14,14,c);Pixel(14,15,c);Pixel(14,16,c);Pixel(14,17,c);Pixel(15,6,c);Pixel(15,7,c);
        Pixel(15,8,c);Pixel(15,9,c);Pixel(15,10,c);Pixel(15,11,c);Pixel(15,12,c);Pixel(15,13,c);Pixel(15,14,c);Pixel(15,15,c);Pixel(15,16,c);Pixel(15,17,c);
        Pixel(16,7,c);Pixel(16,8,c);Pixel(16,9,c);Pixel(16,10,c);Pixel(16,11,c);Pixel(16,12,c);Pixel(16,13,c);Pixel(16,14,c);Pixel(16,15,c);Pixel(16,16,c);
        Pixel(16,17,c);Pixel(17,7,c);Pixel(17,8,c);Pixel(17,9,c);Pixel(17,10,c);Pixel(17,11,c);Pixel(17,12,c);Pixel(17,13,c);Pixel(17,14,c);Pixel(17,15,c);
        Pixel(17,16,c);Pixel(17,17,c);Pixel(18,7,c);Pixel(18,8,c);Pixel(18,9,c);Pixel(18,10,c);Pixel(18,11,c);Pixel(18,12,c);Pixel(18,13,c);Pixel(18,14,c);
        Pixel(18,15,c);Pixel(18,16,c);Pixel(18,17,c);Pixel(19,7,c);Pixel(19,8,c);Pixel(19,9,c);Pixel(19,10,c);Pixel(19,11,c);Pixel(19,12,c);Pixel(19,13,c);
        Pixel(19,14,c);Pixel(19,15,c);Pixel(19,16,c);Pixel(19,17,c);Pixel(20,7,c);Pixel(20,8,c);Pixel(20,9,c);Pixel(20,10,c);Pixel(20,11,c);Pixel(20,12,c);
        Pixel(20,13,c);Pixel(20,14,c);Pixel(20,15,c);Pixel(20,16,c);Pixel(20,17,c);Pixel(21,7,c);Pixel(21,8,c);Pixel(21,9,c);Pixel(21,10,c);Pixel(21,11,c);
        Pixel(21,12,c);Pixel(21,13,c);Pixel(21,14,c);Pixel(21,15,c);Pixel(21,16,c);Pixel(22,7,c);Pixel(22,8,c);Pixel(22,9,c);Pixel(22,10,c);Pixel(22,11,c);
        Pixel(22,12,c);Pixel(22,13,c);Pixel(22,14,c);Pixel(22,15,c);Pixel(23,7,c);Pixel(23,8,c);Pixel(23,9,c);Pixel(23,10,c);Pixel(23,11,c);Pixel(24,7,c);
        Pixel(24,8,c);Pixel(24,9,c);Pixel(24,10,c);Pixel(24,11,c);]))

// parser for a head
let pHead : Parser<PixelPunk> = pbetween (pleft (pstr "Head{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pHeadHelper

// helper function that contains the pixel list for a head outline
let pHeadOutlineHelper = (fun c -> Attribute([Pixel(6,9,c);Pixel(6,10,c);Pixel(6,11,c);Pixel(6,12,c);Pixel(6,13,c);Pixel(6,14,c);Pixel(6,15,c);Pixel(7,8,c);
        Pixel(7,16,c);Pixel(8,7,c);Pixel(8,17,c);Pixel(9,7,c);Pixel(9,17,c);Pixel(10,7,c);Pixel(10,17,c);Pixel(11,7,c);Pixel(11,17,c);Pixel(12,7,c);
        Pixel(12,17,c);Pixel(13,6,c);Pixel(13,17,c);Pixel(14,6,c);Pixel(14,17,c);Pixel(15,6,c);Pixel(15,7,c);Pixel(15,17,c);Pixel(16,7,c);Pixel(16,17,c);
        Pixel(17,7,c);Pixel(17,17,c);Pixel(18,7,c);Pixel(18,17,c);Pixel(19,7,c);Pixel(19,17,c);Pixel(20,7,c);Pixel(20,17,c);Pixel(21,7,c);Pixel(21,16,c);Pixel(22,7,c);
        Pixel(22,11,c);Pixel(22,12,c);Pixel(22,13,c);Pixel(22,14,c);Pixel(22,15,c);Pixel(23,7,c);Pixel(23,11,c);Pixel(24,7,c);Pixel(24,11,c);]))

// parser for outlining the head 
let pHeadOutline : Parser<PixelPunk> = pbetween (pleft (pstr "HeadOutline{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pHeadOutlineHelper

// helper function that contains the pixel list for a pair of sunglasses
let pSunglassesHelper = (fun c -> Attribute([Pixel(13,6,c);Pixel(13,7,c);Pixel(13,8,c);Pixel(13,9,c);Pixel(13,10,c);Pixel(13,11,c);Pixel(13,12,c);Pixel(13,13,c);
        Pixel(13,14,c);Pixel(13,15,c);Pixel(13,16,c);Pixel(13,17,c);Pixel(13,18,c);Pixel(14,9,c);Pixel(14,10,c);Pixel(14,11,c);Pixel(14,12,c);Pixel(14,15,c);Pixel(14,16,c);
        Pixel(14,17,c);Pixel(14,18,c);Pixel(15,10,c);Pixel(15,11,c);Pixel(15,16,c);Pixel(15,17,c);]))

// parser for a pair of sunglasses
let pSunglasses : Parser<PixelPunk> = pbetween (pleft (pstr "Sunglasses{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pSunglassesHelper

// helper function that contains the pixel list for some crazy hair
let pCrazyHairHelper = (fun c -> Attribute([Pixel(1,10,c);Pixel(1,11,c);Pixel(2,4,c);Pixel(2,6,c);Pixel(2,8,c);Pixel(2,9,c);Pixel(2,10,c);Pixel(2,11,c);Pixel(2,12,c);Pixel(2,13,c);
        Pixel(2,15,c);Pixel(2,17,c);Pixel(2,19,c);Pixel(3,5,c);Pixel(3,6,c);Pixel(3,7,c);Pixel(3,8,c);Pixel(3,9,c);Pixel(3,10,c);Pixel(3,11,c);Pixel(3,12,c);Pixel(3,13,c);
        Pixel(3,14,c);Pixel(3,15,c);Pixel(3,16,c);Pixel(3,17,c);Pixel(3,18,c);Pixel(4,4,c);Pixel(4,6,c);Pixel(4,7,c);Pixel(4,8,c);Pixel(4,9,c);Pixel(4,10,c);Pixel(4,11,c);
        Pixel(4,12,c);Pixel(4,13,c);Pixel(4,14,c);Pixel(4,15,c);Pixel(4,16,c);Pixel(4,17,c);Pixel(4,18,c);Pixel(4,20,c);Pixel(5,2,c);Pixel(5,5,c);Pixel(5,6,c);Pixel(5,7,c);
        Pixel(5,8,c);Pixel(5,9,c);Pixel(5,10,c);Pixel(5,11,c);Pixel(5,12,c);Pixel(5,13,c);Pixel(5,14,c);Pixel(5,15,c);Pixel(5,16,c);Pixel(5,17,c);Pixel(5,18,c);Pixel(5,19,c);
        Pixel(6,3,c);Pixel(6,4,c);Pixel(6,6,c);Pixel(6,7,c);Pixel(6,8,c);Pixel(6,9,c);Pixel(6,10,c);Pixel(6,11,c);Pixel(6,12,c);Pixel(6,13,c);Pixel(6,14,c);Pixel(6,15,c);
        Pixel(6,16,c);Pixel(6,17,c);Pixel(6,18,c);Pixel(6,19,c);Pixel(6,21,c);Pixel(7,4,c);Pixel(7,5,c);Pixel(7,6,c);Pixel(7,7,c);Pixel(7,8,c);Pixel(7,16,c);Pixel(7,17,c);
        Pixel(7,18,c);Pixel(7,19,c);Pixel(8,3,c);Pixel(8,4,c);Pixel(8,5,c);Pixel(8,6,c);Pixel(8,7,c);Pixel(8,17,c);Pixel(8,18,c);Pixel(8,19,c);Pixel(8,20,c);Pixel(9,2,c);
        Pixel(9,3,c);Pixel(9,5,c);Pixel(9,6,c);Pixel(9,7,c);Pixel(9,17,c);Pixel(9,18,c);Pixel(9,19,c);Pixel(10,4,c);Pixel(10,5,c);Pixel(10,6,c);Pixel(10,7,c);Pixel(10,17,c);
        Pixel(10,18,c);Pixel(10,19,c);Pixel(10,20,c);Pixel(11,3,c);Pixel(11,4,c);Pixel(11,5,c);Pixel(11,6,c);Pixel(11,7,c);Pixel(11,17,c);Pixel(11,18,c);Pixel(11,19,c);
        Pixel(11,21,c);Pixel(12,2,c);Pixel(12,4,c);Pixel(12,5,c);Pixel(12,6,c);Pixel(12,7,c);Pixel(12,17,c);Pixel(12,18,c);Pixel(12,19,c);Pixel(12,20,c);Pixel(12,22,c);
        Pixel(13,4,c);Pixel(13,5,c);Pixel(13,18,c);Pixel(13,19,c);Pixel(14,3,c);Pixel(14,4,c);Pixel(14,5,c);Pixel(14,18,c);Pixel(14,19,c);Pixel(14,20,c);Pixel(14,21,c);
        Pixel(15,2,c);Pixel(15,4,c);Pixel(15,5,c);Pixel(15,18,c);Pixel(15,19,c);Pixel(15,20,c);Pixel(16,4,c);Pixel(16,5,c);Pixel(16,6,c);Pixel(16,18,c);Pixel(16,21,c);
        Pixel(17,3,c);Pixel(17,5,c);Pixel(17,6,c);Pixel(17,19,c);Pixel(18,4,c);Pixel(18,6,c);]))

// parser for crazy hair
let pCrazyHair : Parser<PixelPunk> = pbetween (pleft (pstr "CrazyHair{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pCrazyHairHelper

// helper function that contains the pixel list for a hat
let pHatHelper = (fun c -> Attribute([Pixel(9,7,c);Pixel(9,8,c);Pixel(9,9,c);Pixel(9,10,c);Pixel(9,11,c);Pixel(9,12,c);Pixel(9,13,c);Pixel(9,14,c);Pixel(9,15,c);
    Pixel(9,16,c);Pixel(9,17,c);Pixel(9,18,c);Pixel(9,19,c);Pixel(9,20,c);Pixel(8,7,c);Pixel(8,8,c);Pixel(8,9,c);Pixel(8,10,c);Pixel(8,11,c);Pixel(8,12,c);
    Pixel(8,13,c);Pixel(8,14,c);Pixel(8,15,c);Pixel(8,16,c);Pixel(8,17,c);Pixel(8,18,c);Pixel(8,19,c);Pixel(7,7,c);Pixel(7,8,c);Pixel(7,9,c);Pixel(7,10,c);
    Pixel(7,11,c);Pixel(7,12,c);Pixel(7,13,c);Pixel(7,14,c);Pixel(7,15,c);Pixel(7,16,c);Pixel(6,8,c);Pixel(6,9,c);Pixel(6,10,c);Pixel(6,11,c);Pixel(6,12,c);
    Pixel(6,13,c);Pixel(6,14,c);Pixel(6,15,c);Pixel(6,16,c);Pixel(5,9,c);Pixel(5,10,c);Pixel(5,11,c);Pixel(5,12,c);Pixel(5,13,c);Pixel(5,14,c);Pixel(5,15,c)]))

// parser for a hat
let pHat : Parser<PixelPunk> = pbetween (pleft (pstr "Hat{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pHatHelper

// helper function that contains the pixel list for a mouth
let pMouthHelper = (fun c -> Attribute([Pixel(19,12,c);Pixel(19,13,c);Pixel(19,14,c);]))

// parser for a mouth
let pMouth : Parser<PixelPunk> = pbetween (pleft (pstr "Mouth{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pMouthHelper

// helper function that contains the pixel list for a nose
let pNoseHelper = (fun c -> Attribute([Pixel(16,13,c);Pixel(16,14,c)]))

// parser for a nose
let pNose : Parser<PixelPunk> = pbetween (pleft (pstr "Nose{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pNoseHelper

// helper function that contains the pixel list for a beard
let pBeardHelper = (fun c -> Attribute([Pixel(15,7,c);Pixel(16,7,c);Pixel(16,8,c);Pixel(17,7,c);Pixel(17,8,c);Pixel(17,9,c);Pixel(17,16,c);Pixel(17,17,c);
    Pixel(18,7,c);Pixel(18,8,c);Pixel(18,9,c);Pixel(18,10,c);Pixel(18,11,c);Pixel(18,12,c);Pixel(18,13,c);Pixel(18,14,c);Pixel(18,15,c);Pixel(18,16,c);
    Pixel(18,17,c);Pixel(19,7,c);Pixel(19,8,c);Pixel(19,9,c);Pixel(19,10,c);Pixel(19,11,c);Pixel(19,15,c);Pixel(19,16,c);Pixel(19,17,c);Pixel(20,9,c);
    Pixel(20,10,c);Pixel(20,11,c);Pixel(20,12,c);Pixel(20,13,c);Pixel(20,14,c);Pixel(20,15,c);Pixel(20,16,c);Pixel(20,17,c);Pixel(21,10,c);Pixel(21,11,c);
    Pixel(21,12,c);Pixel(21,13,c);Pixel(21,14,c);Pixel(21,15,c);Pixel(21,16,c);Pixel(22,11,c);Pixel(22,12,c);Pixel(22,13,c);Pixel(22,14,c);
    Pixel(22,15,c);]))

// parser for a beard
let pBeard : Parser<PixelPunk> = pbetween (pleft (pstr "Beard{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pBeardHelper

// helper function that contains the pixel list for a pair of movie glasses
let pMovieGlassesHelper = (fun c -> Attribute([Pixel(12,6,c);Pixel(12,7,c);Pixel(12,8,c);Pixel(12,9,c);Pixel(12,10,c);Pixel(12,11,c);Pixel(12,12,c);Pixel(12,13,c);
        Pixel(12,14,c);Pixel(12,15,c);Pixel(12,16,c);Pixel(12,17,c);Pixel(12,18,c);Pixel(13,7,c);Pixel(13,8,c);Pixel(13,9,c);Pixel(13,10,Color("27A9F5"));Pixel(13,11,Color("27A9F5"));
        Pixel(13,12,Color("27A9F5"));Pixel(13,13,c);Pixel(13,14,Color("F51B1B"));Pixel(13,15,Color("F51B1B"));Pixel(13,16,Color("F51B1B"));Pixel(13,17,c);Pixel(13,18,c);Pixel(14,8,c);
        Pixel(14,9,c);Pixel(14,10,Color("27A9F5"));Pixel(14,11,Color("27A9F5"));Pixel(14,12,Color("27A9F5"));Pixel(14,13,c);Pixel(14,14,Color("F51B1B"));Pixel(14,15,Color("F51B1B"));
        Pixel(14,16,Color("F51B1B"));Pixel(14,17,c);Pixel(14,18,c);Pixel(15,8,c);Pixel(15,9,c);Pixel(15,10,c);Pixel(15,11,c);Pixel(15,12,c);Pixel(15,13,c);Pixel(15,14,c);Pixel(15,15,c);
        Pixel(15,16,c);Pixel(15,17,c);Pixel(15,18,c);]))

// parser for movie glasses
let pMovieGlasses : Parser<PixelPunk> = pbetween (pleft (pstr "MovieGlasses{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pMovieGlassesHelper

// helper function that contains the pixel list for eyes
let pEyesHelper = (fun c -> Attribute([Pixel(13,9,c);Pixel(13,10,c);Pixel(13,14,c);Pixel(13,15,c);Pixel(14,9,Color("000000"));Pixel(14,10,c);Pixel(14,14,Color("000000"));Pixel(14,15,c);]))

// parser for eyes
let pEyes : Parser<PixelPunk> = pbetween (pleft (pstr "Eyes{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pEyesHelper

// helper function that contains the pixel list for a ponytail
let pPonytailHelper = (fun c -> Attribute([Pixel(2,6,c);Pixel(2,7,c);Pixel(2,8,c);Pixel(3,5,c);Pixel(3,6,c);Pixel(3,7,c);Pixel(3,8,c);Pixel(3,9,c);Pixel(4,5,c);
    Pixel(4,6,c);Pixel(4,7,c);Pixel(4,8,c);Pixel(4,9,c);Pixel(5,5,c);Pixel(5,6,c);Pixel(5,7,c);Pixel(5,8,Color("000000"));Pixel(5,9,Color("000000"));Pixel(5,10,c);Pixel(5,11,c);Pixel(5,12,c);
    Pixel(5,13,c);Pixel(5,14,c);Pixel(6,6,c);Pixel(6,7,Color("000000"));Pixel(6,8,c);Pixel(6,9,c);Pixel(6,10,c);Pixel(6,11,c);Pixel(6,12,c);Pixel(6,13,c);Pixel(6,14,c);Pixel(6,15,c);
    Pixel(7,7,c);Pixel(7,8,c);Pixel(7,9,c);Pixel(7,16,c);Pixel(8,7,c);Pixel(8,8,c);Pixel(8,17,c);Pixel(9,7,c);Pixel(10,7,c);Pixel(11,7,c);Pixel(12,7,c);]))

// parser for a ponytail
let pPonytail : Parser<PixelPunk> = pbetween (pleft (pstr "Ponytail{") (pmany0 pnl)) (pright (pmany0 pnl) (pchar '}')) pColor |>> pPonytailHelper


(* pvar
 *   Parses a variable.  Variable names are at least one
 *   character long, starting with a letter, followed by
 *   any combination of letters or numbers.
 *)
let pVarChar: Parser<char> = pletter <|> pdigit
let pVariable: Parser<PixelPunk> = pseq pletter (pmany0 pVarChar |>> stringify) (fun (c: char, s: string) -> (string c) + s) |>> (fun v -> Variable v)

(* passign
 *   Parses an assignment, e.g.,
 *   x := 2
 *)
let pAssignment = pseq (pleft (pad pVariable) (pchar '=')) (pad pPixelPunk) (fun(x, y) -> Assignment(x, y))

(* pSequenceHelper
 *   Parses any higher-level PixelPunk elements to construct a Sequence of them 
 *)
let pSequenceHelper = (pmany1 (pleft (pCanvas <|> pAttribute <|> pHead <|> pHeadOutline <|> pSunglasses <|> pCrazyHair <|> pPonytail <|> pHat
                        <|> pEyes <|> pMovieGlasses <|> pNose <|> pMouth <|> pBeard<|> pAssignment <|> pVariable) (pmany0 pnl)) |>> (fun p -> Sequence p))
(* pSequence
 *   Checks for "PixelPunk<n>" where n is the output of pSequenceHelper
 *)
let pSequence : Parser<PixelPunk> = pbetween (pleft (pstr "PixelPunk<") (pad (pmany0 pnl))) (pbetween (pad (pmany0 pnl)) (pad (pmany0 pnl)) (pchar '>')) pSequenceHelper


pPixelPunkImpl := pColor <|> pPixel <|> pCanvas <|> pAttribute <|> pHead <|> pHeadOutline <|> pCrazyHair <|> pPonytail
                     <|> pMouth <|> pEyes <|> pMovieGlasses <|> pNose <|> pSunglasses <|> pBeard <|> pHat <|> pSequence <|> pVariable <|> pAssignment

let grammar : Parser<PixelPunk> = pleft pPixelPunk peof

let parse(program: string) : PixelPunk option =
    match grammar (prepare program) with
    | Success(res, _) -> Some(res)
    | Failure(_, _) -> None

