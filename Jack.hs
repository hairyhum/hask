{-# LANGUAGE TemplateHaskell #-}
module Jack where
import Language.Haskell.SealModule

data Tell = Lie | CanLie | Truth
data Sanity = Insane | Sane
data Card = Ace | Two | Three | Four | Five | Six | Seven | Jack
data About = About
data As = As
data Not = Not
data Both = Both

type AceSanity = Sanity
type TwoSanity = Sanity
type Config = (AceSanity, TwoSanity, Bool)

think_about :: Card -> [Card]
think_about Three = [Ace]
think_about Seven = [Five]
think_about Six = [Ace,Two]
think_about Four = [Three,Two]
think_about Five = [Ace,Four]
think_about Jack = [Six,Seven]
think_about _ = []

aceSanity :: Config -> Sanity
aceSanity (ace,_,_) = ace

twoSanity :: Config -> Sanity
twoSanity (_,two,_) = two

hardJudge :: Config -> Bool
hardJudge (_,_,judge) = judge

not' :: Sanity -> Sanity
not' Sane = Insane
not' Insane = Sane

instance Show Card where
    show Ace = "Ace"
    show Two = "Two"
    show Three = "Three"
    show Four = "Four"
    show Five = "Five"
    show Six = "Six"
    show Seven = "Seven"
    show Jack = "Jack"

instance Show Sanity where
    show Sane = "Sane"
    show Insane = "Insane"

is :: Sanity -> Sanity -> Bool
is Sane Sane = True
is Insane Insane = True
is _ _ = False

sealModule [d|
    config :: Config
    config = sealedParam

    insane :: Card -> Bool
    insane Ace = (aceSanity config) `is` Insane
    insane Two = (twoSanity config) `is` Insane
    insane card =
        let judge = hardJudge config
        in if judge
            then card `can` Lie
            else card `only` Lie

    sane :: Card -> Bool
    sane card = not $ insane card

    only :: Card -> Tell -> Bool
    card `only` tell =
        let op = opinions card tell
        in foldl (&&) True op

    can :: Card -> Tell -> Bool
    card `can` tell =
        let op = opinions card tell
        in foldl (||) False op

    opinions :: Card -> Tell -> [Bool]
    opinions card tell =
        map (\other -> has card tell About other) $ think_about card

    has :: Card -> Tell -> About -> Card -> Bool
    has card Lie About other =
        let thought = card `think` other
        in thought Sane == insane other && thought Insane == sane other
    has card Truth About other =
        not (has card Lie About other)

    think :: Card -> Card -> Sanity -> Bool
    Three `think` Ace = is Insane
    Seven `think` Five = is Insane
    Six `think` Ace = is Sane
    Six `think` Two = is Sane
    Five `think` Ace = same As Four
    Five `think` Four = same As Ace
    Four `think` Three = with Two Not Both
    Four `think` Two = with Three Not Both
    Jack `think` Six = with Seven Not Both
    Jack `think` Seven = with Six Not Both

    card `think` other =
        card `not_think` other

    same :: As -> Card -> (Sanity -> Bool)
    same As card Insane = insane card
    same As card Sane = sane card

    with :: Card -> Not -> Both -> Sanity -> Bool
    with card Not Both Insane = sane card
    with card Not Both Sane = True

    not_think :: Card -> Card -> Sanity -> Bool
    not_think card other sanity =
        not $ (card `think` other) (not' sanity)

    sanity card =
        if sane card then Sane else Insane

    |]

foreach = flip mapM_

sanities =
    let configs = [(Sane,Sane,True), (Sane,Insane,True), (Insane,Sane,True),(Insane,Insane,True),
                    (Sane,Sane,False), (Sane,Insane,False), (Insane,Sane,False),(Insane,Insane,False)]
    in foreach configs show_sanities

show_sanities config =
    do let cards = [Ace , Two , Three , Four , Five , Six , Seven , Jack]
       putStrLn "========="
       putStrLn (if hardJudge config then "Hard Judge: \n" else "Soft Judge: \n")
       foreach cards (\card -> print_sanity card (sanity config card))

print_sanity :: Card -> Sanity -> IO ()
print_sanity card sanity =
    putStrLn (show card ++ " is " ++ show sanity)