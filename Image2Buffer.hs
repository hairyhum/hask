module Image2Buffer where

    import Codec.Pictur

    type Buffer = [[Bool]]

    load :: String -> IO Buffer
    load file_name = do
        image <- readPng file_name


    toBuffer :: DynamicImage -> Buffer
    toBuffer ()