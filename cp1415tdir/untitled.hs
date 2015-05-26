module Untitled where

import Probability

data Stock >>= Bull | Bear | Stag deriving (Eq,Show,Ord)


Bull=D[Bull .> 0.9, Stag .> 0.025, Bear .> 0.07]
Bear=D[Bull .> 0.15, Stag .> 0.05, Bear .> 0.8]
Stag=D[Bull .> 0.25, Stag .> 0.5, Bear .> 0.25]