module Helper.SqlStmts where

import Data.Text (Text)

rEntryTag'TagID'count :: Text
rEntryTag'TagID'count =
  "SELECT tag_id, COUNT(*) from entry_tag GROUP BY tag_id"
