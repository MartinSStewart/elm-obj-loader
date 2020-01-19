module OBJ.InternalTypes exposing (Face(..), FaceTriangle(..), Group(..), Int2, Int3, MeshT(..), MeshWithT, TextLine(..), ThreeOrFour(..), VertexWithTextureAndTangentT)

import Array exposing (Array)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import OBJ.Types exposing (..)


type MeshT
    = WithoutTextureT (MeshWith Vertex)
    | WithTextureT (MeshWith VertexWithTexture)
    | WithTextureAndTangentT (MeshWithT VertexWithTextureAndTangentT)
      -- We use this mesh type if we need to parse some lines and don't know what kind of face data we are dealing with yet
    | UnknownMeshType (MeshWith ())


type alias MeshWithT a =
    { vertices : Array a
    , indices : List Int3
    , lines : List Line
    }


type alias VertexWithTextureAndTangentT =
    { position : Vec3, texCoord : Vec2, normal : Vec3, sdir : Vec3, tdir : Vec3 }


type
    TextLine
    -- v 1 3 4
    = V Vec3
      -- vt 2 4
    | Vt Vec2
      -- vn 3 3 1
    | Vn Vec3
      -- f 1 2 4
      -- f 1/3 2/3 1/7
      -- f 1/2/3 7/4/2 8/12/90
      -- f 4//8 4//1 6//2
    | F Face
      -- l 1 2
      -- l 1 4 5
      -- l 1 4 5 10
    | L Line
      -- stateful stuff
    | Object String
    | Group String
    | Smooth String
    | MtlLib String
    | UseMtl String


type Face
    = FVertexTextureNormal (ThreeOrFour Int3)
    | FVertexNormal (ThreeOrFour Int2)


type FaceTriangle
    = FTVertexTextureNormal ( Int3, Int3, Int3 )
    | FTVertexNormal ( Int2, Int2, Int2 )


type ThreeOrFour t
    = Three { a : t, b : t, c : t }
    | Four
        { a : t
        , b : t
        , c : t
        , d : t
        }


type Group
    = GV { faces : List Int3 }
    | GVT { faces : List ( Int2, Int2, Int2 ) }
    | GVTN { faces : List ( Int3, Int3, Int3 ) }
    | GVN { faces : List ( Int2, Int2, Int2 ) }


type alias Int2 =
    ( Int, Int )


type alias Int3 =
    ( Int, Int, Int )
