> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

> module Syntax where

> Lambda := Variable | Application | LambdaAbstraction | ( Lambda )

> Application 
>    := Variable
>     | Variable Application
>     | Variable (Lambda)

> LambdaAbstraction := \\ Variable . Lambda

> Variable := [a-zA-Z]+