module Geometry
    ( triangleA
    , triangleB
    ) where

import LambdaCube.GL.Mesh as LambdaCubeGL
import Data.Map as Map
import Data.Vector as V

-- geometry data: triangles
triangleA, triangleB :: (Num t1, Num t) 
                            => (V.Vector a
                            -> LambdaCubeGL.MeshAttribute)
                            -> (t1 -> t -> a)
                            -> LambdaCubeGL.Mesh
triangleA a v = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  a $ V.fromList [v 1 1, v 1 (-1), v (-1) (-1)])
        , ("uv",        a $ V.fromList [v 1 1, v 0 1, v 0 0]) 
        ]
    , mPrimitive    = P_Triangles
    }   

triangleB a v = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  a $ V.fromList [v 1 1, v (-1) (-1), v (-1) 1]) 
        , ("uv",        a $ V.fromList [v 1 1, v 0 0, v 1 0]) 
        ]
    , mPrimitive    = P_Triangles
    }  
