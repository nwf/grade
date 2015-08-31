-- Header -------------------------------------------------------------- {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Grade.Types where

import qualified Control.Lens.TH   as LTH
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Typeable (Typeable)

------------------------------------------------------------------------ }}}
-- Defines ------------------------------------------------------------- {{{

newtype DingName = DN { unDN :: Text }
 deriving (Eq,Ord,Show,Typeable)

-- | A point deduction definition
--
-- A Ding is parameterized by some modifier for its section.
-- All dings have text to display and a list of comments for
-- internal use.
--
-- Each Ding is associated with a location (parameterized to avoid
-- dependency on any particular parsing framework)
--
data Ding sdt loc = Ding
  { _ding_mod           :: sdt
  , _ding_loc           :: loc
  , _ding_multiple      :: Bool
  , _ding_text          :: Text
  , _ding_comment_lines :: [Text]
  }
 deriving (Eq,Ord,{-Show-}Typeable)
$(LTH.makeLenses ''Ding)

-- | A Section is mostly a collection of Dings.
--
-- Each section, like each Ding, may have a collection of comments
-- associated with it.
--
-- It also conains a section scoring function, which
-- reduces dingmods to a score.
data Section sdt loc = Sec
  { _sec_title         :: Text
  , _sec_max           :: Double
  , _sec_hidden        :: Bool
  , _sec_scorefn       :: sdt -> Either String Double
  , _sec_dingprinter   :: sdt -> Maybe String
  , _sec_dings         :: Map DingName (Ding sdt loc)
  , _sec_comment_lines :: [Text]
  }
 deriving (Typeable)
$(LTH.makeLenses ''Section)

{-
instance (Show sdt, Show loc) => Show (Section sdt loc) where
  show (Sec t m h _ d c) = "Section "
    ++ (show t) ++ " "
    ++ (show m) ++ " " 
    ++ (show h) ++ " "
    ++ "<fun> "
    ++ (show d) ++ " "
    ++ (show c)
-}

-- | Existentially quantify the section data type for a given section
data ExSection loc = forall sdt . ({-Show sdt,-} Monoid sdt) => ExSec (Section sdt loc)

{-
instance (Show loc) => Show (ExSection loc) where
  show es = case es of ExSec s -> show s
-}

-- | A Section Callback object, as returned by a section type parser
data SecCallback f = forall sps sdt . ({-Show sdt,-} Monoid sdt, Monoid sps) =>
  SC
  { -- | Parse section-specific ding weights
    sc_ding_parse :: f (sdt,sps)
  , -- | Optional printout of the sdt data, given
    -- the section's maximum and final sps.
    sc_show_sdt :: sps -> sdt -> Maybe String
  , -- | Scoring function, given section maximum
    -- value and the monoidal summary of section-specific dings
    sc_score :: sps -> sdt -> Either String Double
  , -- | Maximum scoring function
    sc_max :: sps -> Double
  }
 deriving (Typeable)

newtype SecName = SN { unSN :: Text }
 deriving (Eq,Ord,Show,Typeable)

-- | Defines is a collection of Sections, with possibly different types of
-- scoring data in each.
--
data Defines loc = Defs
  { _def_sections :: Map SecName (ExSection loc, loc)
  }
 deriving ({-Show,-} Typeable)
$(LTH.makeLenses ''Defines)

------------------------------------------------------------------------ }}}
-- Data ---------------------------------------------------------------- {{{

data DataFileSection loc = DFS
  { _dfs_secname         :: SecName
  , _dfs_secloc          :: loc
  , _dfs_dings           :: [(DingName,loc)]
  , _dfs_grader_comments :: Maybe Text
  }
 deriving (Show, Typeable)
$(LTH.makeLenses ''DataFileSection)

-- | A report for a student, as produced by a TA
newtype DataFile loc = DF [DataFileSection loc]
 deriving (Show, Typeable)

------------------------------------------------------------------------ }}}
-- Reports ------------------------------------------------------------- {{{

data SectionError loc =
    -- | An unknown ding directive is encountered in the DataFile
    SEUndefinedDing DingName loc
    -- | A ding which was not declared as multiple-use occured twice
  | SEDuplicateDing DingName loc loc
    -- | A scoring error occurred
  | SEScoreError String
 deriving (Show)

data ReportError loc =
    -- | A section is defined in the Defines but is not present in the
    -- DataFile
    REMissingSections (Set SecName)
    -- | A section is invoked twice in the DataFile
  | REDuplicateSection SecName loc loc
    -- | An unknown section directive is encountered in the DataFile
  | REUnknownSection SecName loc
    -- | Error(s) occurring in a particular section
  | RESectionError SecName [SectionError loc]
 deriving (Show)

data ReportFileSection = RFS
  { _rfs_sectitle :: Text
  , _rfs_score    :: Double
  , _rfs_max      :: Double
  , _rfs_dingtext :: [Text]
  , _rfs_comments :: Maybe Text
  }
 deriving (Show, Typeable)
$(LTH.makeLenses ''ReportFileSection)

-- | A report for a student, as they see it
newtype ReportFile = RF [ReportFileSection]

------------------------------------------------------------------------ }}}
