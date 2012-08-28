{-# LANGUAGE RankNTypes,TypeOperators #-}
module Image2Buffer where

    import Codec.Picture
    import Codec.Picture.Types
    import Data.Vector.Storable (map,toList)
    import Data.List

    type Buffer = [[Bool]]

    toBuffer :: LumaPlaneExtractable a => Image a -> Buffer
    toBuffer img = 
        let luma = extractLumaPlane img
            lumaData = imageData luma
            width = imageWidth luma
        in chunk  width $ toList $ Data.Vector.Storable.map ((<) 128) lumaData
    
    chunk :: Int -> [e] -> [[e]]
    chunk i ls = 
        let splitter [] _ n = n
            splitter l c n  = l `c` splitter (drop i l) c n
        in Data.List.map (take i) (build (splitter ls))
    
    build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
    build g = g (:) []

