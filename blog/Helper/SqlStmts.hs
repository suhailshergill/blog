{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Helper.SqlStmts where

import Prelude
import Helper.SqlCombinators


rEntryTag'TagId'count = unQuery (
  groupBy ["tag_id"] $
  select "entry_tag" ["tag_id", "COUNT(*) AS count"]
  )

rEntryTag'TagId'count'Asc_count = unQuery (
  orderBy ASC ["count"] $
  ÑQuery rEntryTag'TagId'count
  )
