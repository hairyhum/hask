module Jack where

data Tell = Lie | CanLie | Truth
data State = Mad | Sane
data Card = Ace | Two | Three | Four | Five | Six | Seven | Jack
data About = About

mad some = some `can` Lie
sane some = some `only` Truth

only :: Card -> Tell -> Bool
only card tell =
    foldl (&&) True (opinions card tell)

can :: Card -> Tell -> Bool
can card tell =
    foldl (||) False (opinions card tell)
    
opinions :: Card -> Tell -> [Bool]
opinions card tell =
    map (\other -> is card tell About other) (tell_about card)

is :: Card -> Tell -> About -> Card -> Bool
is card Lie About other =
    think card other Sane == mad other && think card other Mad == sane other
is card Truth About other =
    not (is card Lie About other)

think :: Card -> Card -> State -> Bool
think Three Ace Mad = True
think Seven Five Mad = True
think Six Ace Sane = True
think Six Two Sane = True
think Four Three Mad = sane Two
think Four Two Mad = sane Three
think Four Three Sane = True
think Four Two Sane = True
think Five Ace Mad = mad Four
think Five Four Mad = mad Ace
think Five Ace Sane = sane Four
think Five Four Sane = sane Ace
think Jack Six Mad = sane Seven
think Jack Seven Mad = sane Six
think Jack Six Sane = True
think Jack Seven Sane = True
think card other state =
    not $ think card other (not' state)

not' :: State -> State
not' Sane = Mad
not' Mad = Sane

tell_about :: Card -> [Card]
tell_about Three = [Ace]
tell_about Seven = [Five]
tell_about Six = [Ace,Two]
tell_about Four = [Three,Two]
tell_about Five = [Ace,Four]
tell_about Jack = [Six,Seven]
tell_about _ = []






