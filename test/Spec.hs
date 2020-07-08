{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Lib

prop_idempotent xs = qsort (qsort xs) == qsort xs

return []
runTests = $quickCheckAll


main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "Passou em todos testes."
                                             else putStrLn "Alguns testes falharam"