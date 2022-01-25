module Ch11
  where 

import Prelude (Unit, show, type (~>), ($))
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
    log "placeholder"
    -- log $ show $ reverse (10 : 20 : 30 : Nil)