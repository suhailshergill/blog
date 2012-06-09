{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Helper.SqlStmts where


rEntryTag'TagID'count =
  "SELECT tag_id, COUNT(*) from entry_tag GROUP BY tag_id"
