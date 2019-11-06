type instruction =
  | Nop
  | Print of IMPExpr.expression
  | Exit
  | Write of IMPExpr.expression * IMPExpr.expression
  | If    of IMPExpr.expression * sequence * sequence
  | While of IMPExpr.expression * sequence
  | Call of IMPExpr.expression (* adresse *)
    * IMPExpr.expression (* fonction *)
    * IMPExpr.expression list (* paramètres *)
  | Return of IMPExpr.expression
and sequence = instruction list
