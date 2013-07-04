(* Copyright (c) 1998 by Lucent Technologies *)

(* cmodel extension signature for empty extension *)

(* for documentation only -- not currently used *)

signature PARSETREEEXT =
sig
  (* DBM: may need equality operations for some or all of these types *)
  type operatorExt = unit
  type ('specifier, 'declatator, 'operator, 'expression, 'statement) expressionExt
  type ('specifier, 'declatator, 'operator, 'expression) specifierExt
  type ('specifier, 'declatator, 'operator, 'expression) declaratorExt
  type ('specifier, 'declatator, 'operator, 'expression, 'statement) statementExt
  type ('specifier, 'declatator, 'operator, 'expression) declarationExt
  type ('specifier, 'declatator, 'operator, 'expression, 'statement) externalDeclExt
end

