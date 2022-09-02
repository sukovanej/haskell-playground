module ParseSql where

type ColumnName = String

data Value = StringValue String | IntValue Int

data Where = Condition

data SQL
  = Select [ColumnName]
  | Insert [ColumnName] [Value]

parseSql :: String -> SQL
parseSql = undefined
