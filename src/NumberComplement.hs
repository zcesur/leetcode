module NumberComplement where

import           Util                           ( toBinary
                                                , fromBinary
                                                )

-- | Given a positive integer, output its complement number. The complement
-- strategy is to flip the bits of its binary representation.
findComplement :: Int -> Int
findComplement = fromBinary . map (1 -) . toBinary
