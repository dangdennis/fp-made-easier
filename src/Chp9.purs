module Ch9 where

import Prelude (Unit, show, ($), (&&), discard, (==))
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do   
    log $ show $ ATrue <> ATrue
    log $ show $ ATrue <> AFalse
    log $ show $ AFalse <> AFalse
    log $ show $ mempty <> ATrue == ATrue
    log $ show $ mempty <> AFalse == AFalse
    verifyAndBoolSemigroup
    verifyAndBoolMonoid
    verifyOrBoolSemigroup
    verifyOrBoolMonoid

class Semigroup a where
    append :: a -> a -> a
infixr 5 append as <>


class Semigroup a <= Monoid a where
    mempty :: a

data AndBool = ATrue | AFalse
derive instance Eq AndBool
derive instance Generic AndBool _

instance Show AndBool where
    show = genericShow

instance Semigroup AndBool where
    append :: AndBool -> AndBool -> AndBool
    append ATrue ATrue = ATrue
    append _ _ = AFalse

instance Monoid AndBool where
    mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
    log "Verifying AndBool Semigroup Laws (1 test)"
    log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
    log "Verifying AndBool Monoid Laws (2 tests)"
    log $ show $ mempty <> ATrue == ATrue <> mempty && ATrue <> mempty == ATrue
    log $ show $ mempty <> AFalse == AFalse <> mempty && AFalse <> mempty == AFalse

data OrBool = OFalse | OTrue
derive instance Eq OrBool
derive instance Generic OrBool _
instance Show OrBool where
    show = genericShow

instance Semigroup OrBool where
    append :: OrBool -> OrBool -> OrBool
    append OFalse OFalse = OFalse
    append _ _ = OTrue

instance Monoid OrBool where
    mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
    log "Verifying OrBool Semigroup Laws (3 tests)"
    log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue)
    log $ show $ (AFalse <> AFalse) <> AFalse == AFalse <> (AFalse <> AFalse)
    log $ show $ (AFalse <> AFalse) <> ATrue == AFalse <> (AFalse <> ATrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
    log "Verifying OrBool Monoid Laws (2 tests)"
    log $ show $ mempty <> OTrue == OTrue <> mempty && OTrue <> mempty == OTrue
    log $ show $ mempty <> OFalse == OFalse <> mempty && OFalse <> mempty == OFalse

data Mod4 = Zero | One | Two | Three

instance Semigroup Mod4 where
    append :: Mod4 -> Mod4 -> Mod4
    append Zero x = x
    append x Zero = x

    append One One = Two
    append One Two = Three
    append One Three = Zero

    append Two One = Three
    append Two Two = Zero
    append Two Three = One

    append Three One = Zero
    append Three Two = One
    append Three Three = Two

instance Monoid Mod4 where
    mempty = Zero

-- todo
-- instance Group Mod4 where


class Monoid a <= Group a where
    ginverse :: a -> a