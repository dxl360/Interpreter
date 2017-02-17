M_interpret
{
 parser
 .....M_statementlist--recursive method
 }

M_statementlist
{
 (cond
  (if .... run M_x return (x,1))
variable declaration	(var variable) or (var variable value)
assignment	(= variable expression)
return	(return expression)
if statement	(if conditional then-statement optional-else-statement)
while statement	(while conditional body-statement
 }

predefine, stmt - expression, boolean, value, 

binding -x-10

tmp - x - 10 

following use tmp
M_var
(
 (lamda (stmt)
M_asg
M_rtn
M_if
M_while
