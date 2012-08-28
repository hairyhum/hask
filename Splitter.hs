module Splitter where

    import Data.Vector.Bit
    import Data.List
    import Data.Maybe

    type Region = [Line]
    type Line = [Bool]

    rows :: Region -> [Line]
    rows = id

    columns :: Region -> [Line]
    columns = transpose

    empty :: Line -> Bool
    empty line = not $ or line

    divide :: Maybe Int -> Maybe Int -> Region -> [Region]
    divide row col region = 
        let parts = divideRow row region
        in concatMap (divideCol col) parts

    divideRow :: Maybe Int -> Region -> [Region]
    divideRow index region = 
        maybe 
            [region] 
            (\index -> [take index region, drop (index + 1) region]) 
            index
    
    divideCol :: Maybe Int -> Region -> [Region]
    divideCol index region = 
        maybe 
            [region]
            (\index -> map transpose $ divideRow (Just index) $ transpose region)
            index

    emptyRow :: Region -> Maybe Int
    emptyRow = findIndex empty

    emptyCol :: Region -> Maybe Int
    emptyCol = findIndex empty . columns

    divideByEmpty :: Region -> [Region]
    divideByEmpty region = 
        let row = emptyRow region
            col = emptyCol region
        in 
        if isNothing col && isNothing row
            then [region]
            else
                filter (not . null) $ concatMap divideByEmpty $ divide row col region

    
