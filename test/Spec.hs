{-# LANGUAGE DataKinds			#-}
{-# LANGUAGE TypeApplications	#-}

import Criterion.Main
import BitVector

--testPlus :: (BitVector 5, BitVector 5) -> BitVector 5
testPlus = uncurry (+)

testPoints :: [(BitVector 64, BitVector 64)]
testPoints = [(1,1),(10,5)]

thing = map (\p -> bench (show p) $ whnf (uncurry (+)) p) testPoints

main = defaultMain [bgroup "(+)" thing]