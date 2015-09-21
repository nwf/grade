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

-- | Things common to dings between their definition and
-- their use
data DingMeta mt = DingMeta
  { _dm_mod  :: mt
  , _dm_text :: Text
  }
 deriving(Eq,Ord,Typeable)
$(LTH.makeLenses ''DingMeta)

-- | A point deduction definition
--
-- A Ding is parameterized by some modifier for its section.
-- All dings have text to display and a list of comments for
-- internal use.
--
-- Each Ding is associated with a location (parameterized to avoid
-- dependency on any particular parsing framework)
--
data DingDefn mt loc = DingDefn
  { _dingd_meta          :: DingMeta mt
  , _dingd_loc           :: loc
  , _dingd_multiple      :: Bool
  , _dingd_comment_lines :: [Text]
  }
 deriving (Eq,Ord,{-Show-}Typeable)
$(LTH.makeLenses ''DingDefn)

data SecMeta sat sdt = SecMeta
  { -- | Title of the section as displayed to the user, not
    -- necessarily the internal name
    _sm_title         :: Text
  , -- | Maximum score
    _sm_max           :: Double
  , -- | Given a reduced sdsdum, format the score for presentation
    -- or indicate that there has been an error.
    _sm_scorefn       :: sat -> sdt -> Either String Double
  , -- | Provide text for printing out the impact of a particular
    -- score adjustment.
    _sm_dingprinter   :: sat -> sdt -> Maybe String
  }
$(LTH.makeLenses ''SecMeta)

-- | A Section is mostly a collection of Dings.
--
-- Each section, like each Ding, may have a collection of comments
-- associated with it.
--
-- It also conains a section scoring function, which
-- reduces dingmods to a score.
data Section f sat sdt loc = Sec
  { _sec_meta          :: SecMeta sat sdt
  , _sec_loc           :: loc
  , _sec_hidden        :: Bool
  , _sec_comment_lines :: [Text]
  , _sec_dings         :: Map DingName (DingDefn sdt loc)
  , _sec_datline_parse :: (Maybe String, f sat)
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
data ExSection f loc = forall sat sdt . ({-Show sdt,-} Monoid sdt) => ExSec (Section f sat sdt loc)

{-
instance (Show loc) => Show (ExSection loc) where
  show es = case es of ExSec s -> show s
-}

-- | A Section Callback object, as returned by a section type parser
data SecCallback f sps sat sdt = SC
  { -- | Section header parser for data file.  This allows one to
    -- have sections whose score is influenced by the @-line in the
    -- *data* file.
    --
    -- The String is for use by the skeleton generator.
    sc_datline_parse :: (Maybe String, f sat)
  , -- | Parse section-specific ding weights
    sc_ding_parse    :: f (sdt,sps)
  , -- | Optional printout of the sdt data, given
    -- the section's final sps.
    sc_show_sdt      :: sps -> sat -> sdt -> Maybe String
  , -- | Scoring function, given section maximum
    -- value and the monoidal summary of section-specific dings
    sc_score         :: sps -> sat -> sdt -> Either String Double
  , -- | Maximum scoring function
    sc_max           :: sps -> Double
  }
 deriving (Typeable)

data ExSecCallback f = forall sps sat sdt . ({-Show sdt,-} Monoid sdt, Monoid sps)
                     => ExSecCB (SecCallback f sps sat sdt)

newtype SecName = SN { unSN :: Text }
 deriving (Eq,Ord,Show,Typeable)

-- | Defines is a collection of Sections, with possibly different types of
-- scoring data in each.
--
-- The same collection is indexed by name and presented in order.
--
data Defines f loc = Defs
  { _def_section_by_name :: Map SecName (ExSection f loc)
  , _def_sections        :: [(SecName, ExSection f loc)]
  }
 deriving ({-Show,-} Typeable)
$(LTH.makeLenses ''Defines)

------------------------------------------------------------------------ }}}
-- Data ---------------------------------------------------------------- {{{

-- | Ding usage by a grader
data DataFileDing mt loc = DFD
  { _dfd_meta          :: DingMeta mt
  , _dfd_loc           :: loc
  }
$(LTH.makeLenses ''DataFileDing)

data DataFileSection sat sdt loc = DFS
  { _dfs_meta            :: SecMeta sat sdt
  , _dfs_sec_arg         :: sat
  , _dfs_loc             :: loc
  , _dfs_dings           :: [DataFileDing sdt loc]
  , _dfs_grader_comments :: Text
  }
 deriving (Typeable)
$(LTH.makeLenses ''DataFileSection)

data ExDFS loc = forall sat sdt . Monoid sdt => ExDFS (DataFileSection sat sdt loc)

-- | A report for a student, as produced by a TA
newtype DataFile loc = DF [(SecName, ExDFS loc)]
 deriving (Typeable)

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
  , _rfs_comments :: Text
  }
 deriving (Show, Typeable)
$(LTH.makeLenses ''ReportFileSection)

-- | A report for a student, as they see it
newtype ReportFile = RF [ReportFileSection]

------------------------------------------------------------------------ }}}
