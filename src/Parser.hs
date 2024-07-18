{-# OPTIONS_GHC -w #-}
module Parser where

import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,117) ([15360,4096,256,0,0,0,0,0,0,0,0,0,0,3584,8191,15360,4096,0,1024,960,256,16,0,60,16,15,49156,3,61441,16384,15360,4096,3840,1024,960,256,240,64,60,16,15,49156,3,61441,16384,15360,4096,3840,1024,960,256,0,32,60,16,15,4,32768,61440,16384,15360,4096,0,512,960,256,0,32,8,0,0,49154,3,61441,16384,0,4096,3840,1024,960,256,0,64,0,8,15,4,32768,0,8192,0,0,0,512,0,0,0,0,0,0,0,2,32768,0,0,0,2048,0,512,0,0,0,32,0,8,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15360,4096,3840,1024,0,0,0,32,0,0,0,2,32768,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExp","Exp","Exps","Binding","Bindings","if","let","while","var","int","t","f","set","begin","vectorref","vector","vectorlength","and","or","not","eq","'+'","'-'","'<'","'>'","')'","'('","%eof"]
        bit_start = st Prelude.* 30
        bit_end = (st Prelude.+ 1) Prelude.* 30
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..29]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (11) = happyShift action_2
action_0 (12) = happyShift action_4
action_0 (13) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (29) = happyShift action_7
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (30) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_20

action_5 _ = happyReduce_7

action_6 _ = happyReduce_8

action_7 (8) = happyShift action_8
action_7 (9) = happyShift action_9
action_7 (10) = happyShift action_10
action_7 (15) = happyShift action_11
action_7 (16) = happyShift action_12
action_7 (17) = happyShift action_13
action_7 (18) = happyShift action_14
action_7 (19) = happyShift action_15
action_7 (20) = happyShift action_16
action_7 (21) = happyShift action_17
action_7 (22) = happyShift action_18
action_7 (23) = happyShift action_19
action_7 (24) = happyShift action_20
action_7 (25) = happyShift action_21
action_7 (26) = happyShift action_22
action_7 (27) = happyShift action_23
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (11) = happyShift action_2
action_8 (12) = happyShift action_4
action_8 (13) = happyShift action_5
action_8 (14) = happyShift action_6
action_8 (29) = happyShift action_7
action_8 (4) = happyGoto action_41
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (29) = happyShift action_40
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (11) = happyShift action_2
action_10 (12) = happyShift action_4
action_10 (13) = happyShift action_5
action_10 (14) = happyShift action_6
action_10 (29) = happyShift action_7
action_10 (4) = happyGoto action_39
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (11) = happyShift action_38
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (11) = happyShift action_2
action_12 (12) = happyShift action_4
action_12 (13) = happyShift action_5
action_12 (14) = happyShift action_6
action_12 (29) = happyShift action_7
action_12 (4) = happyGoto action_34
action_12 (5) = happyGoto action_37
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (11) = happyShift action_2
action_13 (12) = happyShift action_4
action_13 (13) = happyShift action_5
action_13 (14) = happyShift action_6
action_13 (29) = happyShift action_7
action_13 (4) = happyGoto action_36
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (11) = happyShift action_2
action_14 (12) = happyShift action_4
action_14 (13) = happyShift action_5
action_14 (14) = happyShift action_6
action_14 (29) = happyShift action_7
action_14 (4) = happyGoto action_34
action_14 (5) = happyGoto action_35
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (11) = happyShift action_2
action_15 (12) = happyShift action_4
action_15 (13) = happyShift action_5
action_15 (14) = happyShift action_6
action_15 (29) = happyShift action_7
action_15 (4) = happyGoto action_33
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (11) = happyShift action_2
action_16 (12) = happyShift action_4
action_16 (13) = happyShift action_5
action_16 (14) = happyShift action_6
action_16 (29) = happyShift action_7
action_16 (4) = happyGoto action_32
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (11) = happyShift action_2
action_17 (12) = happyShift action_4
action_17 (13) = happyShift action_5
action_17 (14) = happyShift action_6
action_17 (29) = happyShift action_7
action_17 (4) = happyGoto action_31
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (11) = happyShift action_2
action_18 (12) = happyShift action_4
action_18 (13) = happyShift action_5
action_18 (14) = happyShift action_6
action_18 (29) = happyShift action_7
action_18 (4) = happyGoto action_30
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (11) = happyShift action_2
action_19 (12) = happyShift action_4
action_19 (13) = happyShift action_5
action_19 (14) = happyShift action_6
action_19 (29) = happyShift action_7
action_19 (4) = happyGoto action_29
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (11) = happyShift action_2
action_20 (12) = happyShift action_4
action_20 (13) = happyShift action_5
action_20 (14) = happyShift action_6
action_20 (29) = happyShift action_7
action_20 (4) = happyGoto action_28
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (11) = happyShift action_2
action_21 (12) = happyShift action_27
action_21 (13) = happyShift action_5
action_21 (14) = happyShift action_6
action_21 (29) = happyShift action_7
action_21 (4) = happyGoto action_26
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (11) = happyShift action_2
action_22 (12) = happyShift action_4
action_22 (13) = happyShift action_5
action_22 (14) = happyShift action_6
action_22 (29) = happyShift action_7
action_22 (4) = happyGoto action_25
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_2
action_23 (12) = happyShift action_4
action_23 (13) = happyShift action_5
action_23 (14) = happyShift action_6
action_23 (29) = happyShift action_7
action_23 (4) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (11) = happyShift action_2
action_24 (12) = happyShift action_4
action_24 (13) = happyShift action_5
action_24 (14) = happyShift action_6
action_24 (29) = happyShift action_7
action_24 (4) = happyGoto action_61
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (11) = happyShift action_2
action_25 (12) = happyShift action_4
action_25 (13) = happyShift action_5
action_25 (14) = happyShift action_6
action_25 (29) = happyShift action_7
action_25 (4) = happyGoto action_60
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (11) = happyShift action_2
action_26 (12) = happyShift action_4
action_26 (13) = happyShift action_5
action_26 (14) = happyShift action_6
action_26 (29) = happyShift action_7
action_26 (4) = happyGoto action_59
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (28) = happyShift action_58
action_27 _ = happyReduce_20

action_28 (11) = happyShift action_2
action_28 (12) = happyShift action_4
action_28 (13) = happyShift action_5
action_28 (14) = happyShift action_6
action_28 (29) = happyShift action_7
action_28 (4) = happyGoto action_57
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (11) = happyShift action_2
action_29 (12) = happyShift action_4
action_29 (13) = happyShift action_5
action_29 (14) = happyShift action_6
action_29 (29) = happyShift action_7
action_29 (4) = happyGoto action_56
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_55
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (11) = happyShift action_2
action_31 (12) = happyShift action_4
action_31 (13) = happyShift action_5
action_31 (14) = happyShift action_6
action_31 (29) = happyShift action_7
action_31 (4) = happyGoto action_54
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (11) = happyShift action_2
action_32 (12) = happyShift action_4
action_32 (13) = happyShift action_5
action_32 (14) = happyShift action_6
action_32 (29) = happyShift action_7
action_32 (4) = happyGoto action_53
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (28) = happyShift action_52
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (11) = happyShift action_2
action_34 (12) = happyShift action_4
action_34 (13) = happyShift action_5
action_34 (14) = happyShift action_6
action_34 (29) = happyShift action_7
action_34 (4) = happyGoto action_34
action_34 (5) = happyGoto action_51
action_34 _ = happyReduce_22

action_35 (28) = happyShift action_50
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (12) = happyShift action_49
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (28) = happyShift action_48
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (11) = happyShift action_2
action_38 (12) = happyShift action_4
action_38 (13) = happyShift action_5
action_38 (14) = happyShift action_6
action_38 (29) = happyShift action_7
action_38 (4) = happyGoto action_47
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (11) = happyShift action_2
action_39 (12) = happyShift action_4
action_39 (13) = happyShift action_5
action_39 (14) = happyShift action_6
action_39 (29) = happyShift action_7
action_39 (4) = happyGoto action_46
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (29) = happyShift action_45
action_40 (6) = happyGoto action_43
action_40 (7) = happyGoto action_44
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (11) = happyShift action_2
action_41 (12) = happyShift action_4
action_41 (13) = happyShift action_5
action_41 (14) = happyShift action_6
action_41 (29) = happyShift action_7
action_41 (4) = happyGoto action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (11) = happyShift action_2
action_42 (12) = happyShift action_4
action_42 (13) = happyShift action_5
action_42 (14) = happyShift action_6
action_42 (29) = happyShift action_7
action_42 (4) = happyGoto action_75
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (29) = happyShift action_45
action_43 (6) = happyGoto action_43
action_43 (7) = happyGoto action_74
action_43 _ = happyReduce_25

action_44 (28) = happyShift action_73
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (11) = happyShift action_2
action_45 (12) = happyShift action_4
action_45 (13) = happyShift action_5
action_45 (14) = happyShift action_6
action_45 (29) = happyShift action_7
action_45 (4) = happyGoto action_72
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (28) = happyShift action_71
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (28) = happyShift action_70
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_4

action_49 (28) = happyShift action_69
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_10

action_51 _ = happyReduce_23

action_52 _ = happyReduce_11

action_53 (28) = happyShift action_68
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (28) = happyShift action_67
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_14

action_56 (28) = happyShift action_66
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (28) = happyShift action_65
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_21

action_59 (28) = happyShift action_64
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (28) = happyShift action_63
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (28) = happyShift action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_19

action_63 _ = happyReduce_18

action_64 _ = happyReduce_17

action_65 _ = happyReduce_16

action_66 _ = happyReduce_15

action_67 _ = happyReduce_13

action_68 _ = happyReduce_12

action_69 _ = happyReduce_9

action_70 _ = happyReduce_3

action_71 _ = happyReduce_6

action_72 (11) = happyShift action_2
action_72 (12) = happyShift action_4
action_72 (13) = happyShift action_5
action_72 (14) = happyShift action_6
action_72 (29) = happyShift action_7
action_72 (4) = happyGoto action_78
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (11) = happyShift action_2
action_73 (12) = happyShift action_4
action_73 (13) = happyShift action_5
action_73 (14) = happyShift action_6
action_73 (29) = happyShift action_7
action_73 (4) = happyGoto action_77
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_26

action_75 (28) = happyShift action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_5

action_77 (28) = happyShift action_80
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (28) = happyShift action_79
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_24

action_80 _ = happyReduce_2

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 7 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Set (Var happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Begin happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 4 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn4
		 (Bool True
	)

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn4
		 (Bool False
	)

happyReduce_9 = happyReduce 5 4 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_4)) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (VectorRef happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 4 4 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Vector happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 4 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (VectorLength happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 4 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (And happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 4 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Or happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 4 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Not happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 4 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Eq happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 5 4 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Plus happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 4 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Minus happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 5 4 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Less happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 5 4 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Greater happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  4 happyReduction_20
happyReduction_20 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn4
		 (Int happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 4 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Negative happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  5 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  5 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 6 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2, happy_var_3)
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  7 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  7 happyReduction_26
happyReduction_26 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenIf -> cont 8;
	TokenLet -> cont 9;
	TokenWhile -> cont 10;
	TokenVar happy_dollar_dollar -> cont 11;
	TokenInt happy_dollar_dollar -> cont 12;
	TokenTrue -> cont 13;
	TokenFalse -> cont 14;
	TokenSet -> cont 15;
	TokenBegin -> cont 16;
	TokenVecRef -> cont 17;
	TokenVector -> cont 18;
	TokenVecLength -> cont 19;
	TokenAnd -> cont 20;
	TokenOr -> cont 21;
	TokenNot -> cont 22;
	TokenEq -> cont 23;
	TokenPlus -> cont 24;
	TokenMinus -> cont 25;
	TokenLess -> cont 26;
	TokenGreater -> cont 27;
	TokenRParen -> cont 28;
	TokenLParen -> cont 29;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 30 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseExp tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Token
  = TokenIf
  | TokenLet
  | TokenWhile
  | TokenVar String
  | TokenInt Int
  | TokenTrue
  | TokenFalse
  | TokenSet
  | TokenBegin
  | TokenVecRef
  | TokenVector
  | TokenVecLength
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenLess
  | TokenGreater
  | TokenRParen
  | TokenLParen
  deriving (Show)

data Exp
  = Var String
  | Let [(Exp, Exp)] Exp 
  | Set Exp Exp
  | Begin [Exp]
  | If Exp Exp Exp
  | While Exp Exp
  | Bool Bool
  | VectorRef Exp Int
  | Vector [Exp]
  | VectorLength Exp
  | And Exp Exp
  | Or Exp Exp
  | Not Exp
  | Eq Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Less Exp Exp
  | Greater Exp Exp
  | Int Int
  | Negative Int
  deriving (Show)

data Exps =
  Exps [Exp]
  
parseError :: [Token] -> a
parseError _ = error "Parse error"

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexExp (c:cs)
  | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreater : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
  where (num, rest) = span isDigit cs

lexExp cs =
  case span isAlpha cs of
  ("if", rest) -> TokenIf : lexer rest
  ("let", rest) -> TokenLet : lexer rest
  ("while", rest) -> TokenWhile : lexer rest
  ("t", rest) -> TokenTrue : lexer rest
  ("f", rest) -> TokenFalse : lexer rest
  ("set", rest) -> TokenSet : lexer rest
  ("begin", rest) -> TokenBegin : lexer rest
  ("eq", rest) -> TokenEq : lexer rest
  ("not", rest) -> TokenNot : lexer rest
  ("and", rest) -> TokenAnd : lexer rest
  ("or", rest) -> TokenOr : lexer rest
  ("vectorlength", rest) -> TokenVecLength : lexer rest
  ("vectorref", rest) -> TokenVecRef : lexer rest
  ("vector", rest) -> TokenVector : lexer rest 
  (var, rest) -> TokenVar var : lexer rest

main = getContents >>= print . parseExp . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
