module Helper.SqlCombinators where

import Prelude hiding ((++))
import Data.Text (Text, append, pack)
import Helper.Utils

(++) :: Text -> Text -> Text
(++) = append

type ÃCol = Text
type ÃTable = Text
newtype ÑQuery = ÑQuery { unQuery :: Text }

select :: ÃTable -> [ÃCol] -> ÑQuery
select table cols = ÑQuery (
  (++)
  (("SELECT " ++) $ listToText cols)
  (" FROM " ++ table)
  )

data Sort = ASC | DESC
          deriving (Read, Show, Enum, Eq, Ord)

groupBy :: [ÃCol] -> ÑQuery -> ÑQuery
groupBy cols query = ÑQuery (
  (unQuery query)
  ++ " GROUP BY "
  ++ (listToText cols)
  )

orderBy :: Sort -> [ÃCol] -> ÑQuery -> ÑQuery
orderBy sort cols query = ÑQuery (
  (unQuery query)
  ++ " ORDER BY "
  ++ (listToText cols)
  ++ (" " ++ (pack $ show sort))
  )
