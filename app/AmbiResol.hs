-- Copyright (c) 2019-2023 China University of Water Resources and Electric Power,
-- All rights reserved.

module AmbiResol (
    LeftExtend,          -- [(Category,Tag,PhraStru)]
    LeftOver,            -- (Category,Tag,PhraStru)
    RightOver,           -- (Category,Tag,PhraStru)
    RightExtend,         -- [(Category,Tag,PhraStru)]
    OverType,            -- Int
    Prior(..),           -- Prior and its all Constructors
    StruGene,            -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    OverPair,            -- (PhraCate, PhraCate, Prior)
    LeftPhra,            -- PhraCate
    RightPhra,           -- PhraCate
    Context,             -- [PhraCate]
    AmbiResol1           -- (LeftPhra, RightPhra, Context, OverType, Prior)
    ) where

import Category
import Phrase (Tag, PhraStru, PhraCate)

{- To indicate which phrasal structure is more prior in an overlapping pair, the left-adjacent phrases and the right-
   adjacent phrases should be considered. As basic fragments, such quadruples would exist in many
   sentences, and act like human body genes.
   The structural gene is a 6-tuple (<leftExtend>, <leftOver>, <rightOver>, <rightExtend>, <overType>, <prior>), here
   <leftExtend> is the left adjacent phrases of <leftOver>, <rightExtend> is the right adjacent phrases of <rightOver>, and
   <leftOver> and <rightOver> are the left-to-right overlapping phrases, with <overType> to indicate overlapping type,
   and with <prior> to indicate which is prior to exist. Besides, <leftOver> and <rightOver> are at least one active.
 -}

type LeftExtend = [(Category,Tag,PhraStru)]     -- Left neighbors
type LeftOver = (Category,Tag,PhraStru)         -- Overlapping left phrase
type RightOver = (Category,Tag,PhraStru)        -- Overlapping right phrase
type RightExtend = [(Category,Tag,PhraStru)]    -- Right neighbors
type OverType = Int                             -- Overlapping type
data Prior = Lp | Rp | Noth deriving (Eq, Read)    -- Lp means left prior, Rp means right prior, Noth means nothing.

instance Show Prior where
    show Lp = "Lp"
    show Rp = "Rp"
    show Noth = "Noth"

-- The structural genes are stored in table stru_gene of MySQL database ccg4c.

type StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)

-- An overlapping pair of phrasal categories, including its priority assignment, used in clause parsing.

type OverPair = (PhraCate, PhraCate, Prior)

{- The following defines No.1 ambiguity resolution model.
 -}

type LeftPhra = PhraCate                       -- Overlapping left phrase
type RightPhra = PhraCate                      -- Overlapping right phrase
type Context = [PhraCate]                      -- All phrases created to now but not including LeftPhra and RightPhra.
type AmbiResol1 = (LeftPhra, RightPhra, Context, OverType, Prior)      -- Ambiguity resolution model
