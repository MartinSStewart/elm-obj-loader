module CombineParser exposing (..)

import Json.Decode as JD
import Combine exposing (..)
import Combine.Num exposing (..)
import Combine.Char exposing (..)
import Regex exposing (find, HowMany(..))
import OBJ.InternalTypes exposing (..)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector2 as V2 exposing (Vec2, vec2)


parse s =
    case Combine.parse file s of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (formatError errors stream)


parseLine l =
    case Combine.parse line l of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (formatError errors stream)


file : Parser s (List Line)
file =
    (many ignoredLines)
        *> sepBy (many1 ignoredLines)
            line
        <* (many ignoredLines)
        <* end


ignoredLines : Parser s ()
ignoredLines =
    (skip eol) <|> (skip comment)


line : Parser s Line
line =
    choice
        [ V <$> vertex
        , Vt <$> vertexTexture
        , Vn <$> vertexNormal
        , F <$> face
        , Object <$> objectName
        , Group <$> group
        , Smooth <$> smooth
        , UseMtl <$> usemtl
        , MtlLib <$> mtllib
        ]
        <* regex "[ \t]*"


objectName : Parser s String
objectName =
    regex "o[ \t]+" *> regex ".+"


mtllib : Parser s String
mtllib =
    regex "mtllib[ \t]+" *> regex ".+"


group : Parser s String
group =
    (regex "g[ \t]+" *> regex ".+")
        <|> (char 'g' *> succeed "")


smooth : Parser s String
smooth =
    regex "s[ \t]+" *> regex ".+"


usemtl : Parser s String
usemtl =
    regex "usemtl[ \t]+" *> regex ".+"


face : Parser s Face
face =
    regex "f[ \t]+"
        *> choice
            [ fVertexTextureNormal
            , fVertexNormal
            , fVertex
            , fVertexTexture
            ]


fVertex : Parser s a
fVertex =
    threeOrFourValues int
        *> fail "Models with no precalculated vertex normals are not supported!"


fVertexTexture : Parser s a
fVertexTexture =
    threeOrFourValues int_int
        *> fail "Models with no precalculated vertex normals are not supported!"


fVertexTextureNormal : Parser s Face
fVertexTextureNormal =
    FVertexTextureNormal <$> threeOrFourValues int_int_int


fVertexNormal : Parser s Face
fVertexNormal =
    FVertexNormal <$> threeOrFourValues int__int


threeOrFourValues : Parser s a -> Parser s (ThreeOrFour a)
threeOrFourValues elements =
    (Four <$> (fourValues (,,,) elements))
        <|> (Three <$> (threeValues (,,) elements))


int_int : Parser s ( Int, Int )
int_int =
    (,) <$> int <*> (string "/" *> int)


int_int_int : Parser s ( Int, Int, Int )
int_int_int =
    (,,) <$> int <*> (string "/" *> int) <*> (string "/" *> int)


int__int : Parser s ( Int, Int )
int__int =
    (,) <$> int <*> (string "//" *> int)


threeValues : (a -> a -> a -> b) -> Parser s a -> Parser s b
threeValues tagger vtype =
    tagger <$> (vtype) <*> (spaces *> vtype) <*> (spaces *> vtype)


fourValues : (a -> a -> a -> a -> b) -> Parser s a -> Parser s b
fourValues tagger vtype =
    tagger <$> (vtype) <*> (spaces *> vtype) <*> (spaces *> vtype) <*> (spaces *> vtype)


vertexNormal : Parser s Vec3
vertexNormal =
    regex "vn[ \t]+" *> (V3.normalize <$> vector3)


vertexTexture : Parser s Vec2
vertexTexture =
    regex "vt[ \t]+" *> ((ignoreZ <$> vector3) <|> vector2)


vertex : Parser s Vec3
vertex =
    regex "v[ \t]+" *> vector3


comment : Parser s String
comment =
    regex "#" *> regex ".*"


vector3 : Parser s Vec3
vector3 =
    threeValues vec3 betterFloat


spaces : Parser s String
spaces =
    regex "[ \t]+"


vector2 : Parser s Vec2
vector2 =
    vec2 <$> betterFloat <*> (spaces *> betterFloat)


betterFloat : Parser s Float
betterFloat =
    (\s -> Result.withDefault 0 (JD.decodeString JD.float s)) <$> regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"


formatError : List String -> InputStream -> String
formatError ms stream =
    let
        location =
            currentLocation stream

        separator =
            "| "

        expectationSeparator =
            "\n  * "

        lineNumberOffset =
            floor (logBase 10 (toFloat location.line)) + 1

        separatorOffset =
            String.length separator

        padding =
            location.column + separatorOffset + 2
    in
        "Parse error around line:\n\n"
            ++ toString location.line
            ++ separator
            ++ location.source
            ++ "\n"
            ++ String.padLeft padding ' ' "^"
            ++ "\nI expected one of the following:\n"
            ++ expectationSeparator
            ++ String.join expectationSeparator ms


toInt : String -> Int
toInt s =
    String.toInt s |> Result.withDefault 0


ignoreZ : Vec3 -> Vec2
ignoreZ v =
    let
        ( x, y, _ ) =
            V3.toTuple v
    in
        vec2 x y
