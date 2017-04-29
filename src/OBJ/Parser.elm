module OBJ.Parser exposing (..)

import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Parser exposing (Parser, (|.), (|=), float, int, succeed, repeat, ignore, ignoreUntil, Count(..), keep, symbol, keyword, oneOf, oneOrMore, delayedCommit, delayedCommitMap, end, zeroOrMore)
import Parser.LanguageKit exposing (whitespace, MultiComment(..), LineComment(..))
import OBJ.InternalTypes exposing (..)
import Regex


-- TODO: figure out how nice error messages work
--
-- The obj specs:
--  http://www.martinreddy.net/gfx/3d/OBJ.spec


parse : String -> Result String (List Line)
parse input =
    -- String.split "\n" input
    -- |> List.foldr parseLineAcc (Ok [])
    Parser.run file input
        |> Result.mapError toString


type Progress
    = InProgress { toDo : List String, done : List Line }
    | Finished (Result String (List Line))


{-| For incremental parsing.
This has to be used first
-}
startParse : String -> Progress
startParse input =
    InProgress { toDo = String.split "\n" input, done = [] }


{-| For incremental parsing.
Do only stepSize steps of parsing.
If stepSize is chosen small enough,
then the browser will not freeze if sleep 0 statements are used inbetween.
-}
stepParse : Int -> Progress -> Progress
stepParse stepSize progress =
    case progress of
        InProgress { toDo, done } ->
            case toDo of
                [] ->
                    Finished (Ok done)

                [ l ] ->
                    if canSkip l then
                        Finished (Ok done)
                    else
                        parseLine l
                            |> Result.map (\d -> done ++ [ d ])
                            |> Finished

                l :: ls ->
                    if canSkip l then
                        -- skipping a line doesn't count towards numbers of lines parsed.
                        -- should it count?
                        stepParse stepSize (InProgress { toDo = ls, done = done })
                    else
                        case parseLine l of
                            Ok d ->
                                let
                                    p =
                                        InProgress { toDo = ls, done = done ++ [ d ] }
                                in
                                    if stepSize <= 0 then
                                        p
                                    else
                                        stepParse (stepSize - 1) p

                            Err e ->
                                Finished (Err e)

        finished ->
            finished


parseLineAcc : String -> Result String (List Line) -> Result String (List Line)
parseLineAcc line acc =
    case acc of
        Ok lines ->
            if canSkip line then
                Ok lines
            else
                parseLine line
                    |> Result.andThen
                        (\l ->
                            Ok (l :: lines)
                        )

        Err e ->
            Err e


canSkip line =
    Regex.contains (Regex.regex "^((\\s*)|(#.*))$") line


parseLine l =
    case Parser.run line l of
        Ok result ->
            Ok result

        Err errors ->
            Err (toString errors)


file : Parser (List Line)
file =
    succeed identity
        |= repeat zeroOrMore
            (succeed identity
                |. repeat zeroOrMore ignoredStuff
                |= line
                |. repeat zeroOrMore ignoredStuff
            )
        |. end


line : Parser Line
line =
    oneOf
        [ vertexTexture
        , vertexNormal
        , vertex
        , face
        , objectName
        , group
        , smooth
        , usemtl
        , mtllib
        ]


objectName : Parser Line
objectName =
    succeed Object
        |. keyword "o"
        |. spaces
        |= words


mtllib : Parser Line
mtllib =
    succeed MtlLib
        |. keyword "mtllib"
        |. spaces
        |= words


group : Parser Line
group =
    oneOf
        [ succeed Group
            |= delayedCommit
                (keyword "g"
                    |. spaces
                )
                words
        , Parser.map (always (Group "")) (keyword "g")
        ]


smooth : Parser Line
smooth =
    succeed Smooth
        |. keyword "s"
        |. spaces
        |= words


usemtl : Parser Line
usemtl =
    succeed UseMtl
        |. keyword "usemtl"
        |. spaces
        |= words


words : Parser String
words =
    keep oneOrMore (\c -> not (c == '\n' || c == '#'))


face : Parser Line
face =
    succeed F
        |. keyword "f"
        |. spaces
        |= oneOf
            [ fVertexNormal
            , fVertexTextureNormal
            ]


fVertexTextureNormal : Parser Face
fVertexTextureNormal =
    succeed FVertexTextureNormal
        |= threeOrFourValues int_int_int


fVertexNormal : Parser Face
fVertexNormal =
    succeed FVertexNormal
        |= threeOrFourValues int__int


threeOrFourValues : Parser a -> Parser (ThreeOrFour a)
threeOrFourValues elements =
    oneOf
        [ delayedCommitMap (\( a, b, c ) d -> Four ( a, b, c, d ))
            (threeValues (,,) elements
                |. spaces
            )
            elements
        , succeed Three
            |= threeValues (,,) elements
        ]


int_int_int : Parser ( Int, Int, Int )
int_int_int =
    -- temporary workaround for
    -- https://github.com/elm-tools/parser/issues/4
    succeed (\a b c -> ( round a, round b, round c ))
        |= float
        |. symbol "/"
        |= float
        |. symbol "/"
        |= float


int__int : Parser ( Int, Int )
int__int =
    -- temporary workaround for
    -- https://github.com/elm-tools/parser/issues/4
    delayedCommitMap (\a b -> ( round a, round b ))
        (succeed identity
            |= float
            |. symbol "//"
        )
        float


int_int : Parser ( Int, Int )
int_int =
    succeed (,)
        |= int
        |. symbol "/"
        |= int


vertexNormal : Parser Line
vertexNormal =
    succeed Vn
        |. keyword "vn"
        |. spaces
        |= (Parser.map V3.normalize vector3)


vertexTexture : Parser Line
vertexTexture =
    succeed Vt
        |. keyword "vt"
        |. spaces
        |= (oneOf [ ignoreZv3, vector2 ])


ignoreZv3 : Parser Vec2
ignoreZv3 =
    delayedCommitMap (\v2 z -> v2)
        vector2
        (succeed identity
            |. spaces
            |= signedFloat
        )


vertex : Parser Line
vertex =
    succeed V
        |. keyword "v"
        |. spaces
        |= vector3


vector3 : Parser Vec3
vector3 =
    threeValues vec3 signedFloat


threeValues : (a -> a -> a -> b) -> Parser a -> Parser b
threeValues tagger vtype =
    succeed tagger
        |= vtype
        |. spaces
        |= vtype
        |. spaces
        |= vtype


fourValues : (a -> a -> a -> a -> b) -> Parser a -> Parser b
fourValues tagger vtype =
    succeed tagger
        |= vtype
        |. spaces
        |= vtype
        |. spaces
        |= vtype
        |. spaces
        |= vtype


vector2 : Parser Vec2
vector2 =
    succeed vec2
        |= signedFloat
        |. spaces
        |= signedFloat


spaces : Parser ()
spaces =
    ignore oneOrMore isSpace


ignoredStuff : Parser ()
ignoredStuff =
    oneOf
        [ delayedCommit (ignore (Exactly 1) isSpace) (ignoreUntil "\n")
        , delayedCommit (ignore (Exactly 1) (\c -> c == '#')) (ignoreUntil "\n")

        -- ignore windows line endings
        -- '\x0D' is \r, elm format somehow thought \x0D was nicer..
        , delayedCommit (ignore (Exactly 1) (\c -> c == '\x0D')) (ignoreUntil "\n")
        , ignore (Exactly 1) (\c -> c == '\n')
        ]


isSpace : Char -> Bool
isSpace char =
    char == ' ' || char == '\t'


signedFloat : Parser Float
signedFloat =
    oneOf
        [ delayedCommit (symbol "-") (Parser.map negate float)
        , delayedCommit (symbol "+") float
        , float
        ]
