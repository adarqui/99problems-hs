module Main (
 b_nextToLast,
 b_nextToLast',
 b_nextToLast'',
 b_nextToLast'''
) where

import Experimentation.P01
import Experimentation.P02
import Experimentation.P03
import Experimentation.P04
import Experimentation.P05
import Experimentation.P06
import Experimentation.P07
import Experimentation.P08
import Experimentation.P09
import Experimentation.P10
import Experimentation.P11
import Experimentation.P12
import Experimentation.P14
import Experimentation.P15
import Experimentation.P16
import Experimentation.P17
import Experimentation.P18
import Experimentation.P19
import Experimentation.P20
import Experimentation.P21
import Experimentation.P22

b_nextToLast = nextToLast_nat_3
b_nextToLast' = nextToLast_foldl
b_nextToLast'' = nextToLast_foldl'
b_nextToLast''' = nextToLast_rec

main :: IO ()
main = do
 p01_test
 p02_test
 p03_test
 p04_test
 p05_test
 p06_test
 p07_test
 p08_test
 p09_test
 p10_test
 p11_test
 p12_test
 p14_test
 p15_test
 p16_test
 p17_test
 p18_test
 p19_test
 p20_test
 p21_test
 p22_test
 
 print "Done."
