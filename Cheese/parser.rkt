#lang racket

(struct If_expression (gaurd ifTrue ifFalse))
(struct Additive_expression (operand primary1 primary2))
(struct Multiplicative_expression (operand primary1 primary2))
(struct Boolean_expression (operand primary1 primary2))
(struct While_statement (gaurd body))
(stuct Enum_statement (identifier cases))
(struct Or_statement (exp1 exp2))
(struct Switch_statement (exp cases default))
(struct Call_statement (identifier expressions))
(struct Assignment_statement (identifier exp))
(struct Primary_Expression (value))