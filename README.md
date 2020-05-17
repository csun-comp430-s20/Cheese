# Cheese
This language was designed to write prolog without having to write prolog. It is implemented in racket and has a recursive flow.

CODE SNIPPETS:
The compiled Cheese code won't run in prolog. My knowledge of prolog at the moment is inadequate to fully implement the language's features.

Closure-

(def int foo (int i)
    {
        (= int p (+ i 1))
        (def int bar (int j)
        {} 
        (+ j p))
    } bar)
    
(= int t (foo 2))
(= int peep (t 3))

The above code translated- 

call_closure(Retval1, Retval2) :- copy_term(Retval1, (Retval2 :- Retval3)), call(Retval3).
foo(I, Retval8) :- P is I+1, Bar = ((J,  Retval10 ), Retval10 is J+P), Retval8 is Bar.
foo(2, Retval11), T = Retval11.
Peep = call_closure(T, 3).


Enum and Switch-

(enum y (case jay case scott))

(switch y.jay
    (case y.jay (t 4))
    (case y.scott (+ peep 6))
    (default (- peep 2)))

The above code translated-

switch(Retval4, [Retval5:Retval6|Retval7]) :- ((Retval4=Retval5 ; Retval5 == "default") -> call(Retval6) ; switch(Retval4, Retval7)).

y(jay).
y(scott).
switch(y(jay), [ y(jay) : call_closure(T, 4), y(scott) : Peep+6, "default" : Peep-2 ]).


Limitations:

The compiled Cheese code does not compile in prolog.
Variables are immutable.
Multiple features have not been implemented in the code generator due to a lack of time and experience. E.g. print statement, while statement, fail statement, Or statement.
Strings and booleans are handled but not fully tested.
Strings cannot be manipulated.
Only integers, not support for float or double.

Changes I would make knowing the info I do now:

Design a high level view of each component of the compiler before writing anything.
I would pick a target language and implementation language everyone in the group was comfortable with.


Running the compiler:

Open Cheese_compiler.rkt with Dr.Racket
Run the file
Enter in the path of the .txt file your Cheese code is written on
The compiled code will be in the same location as the .txt file entered and have the same name but will have an extension of .pl
The .pl file can be ran with swi-prolog

Syntax:

i ∈ int
x ∈ variable
s ∈ String ::= “...”
v ∈ Type ::= int | String | boolean
op ::= + | - | * | / | < | = | > | == | and
e ∈ exp ::= (if (e) e else e) | (if (not (e)) e else e) | a 
a ∈ addition ::=  (+ p p) |  (- p p) | m
m ∈ multiplication ::=  (* p p) |  (/ p p) | b
b ∈ lessthan ::=  (< p p) |  (> p p) | c
c ∈ lessthanequal ::= (<= p p) | (>= p p) | k
k ∈ equivalence ::= (== p p) | l
l ∈ logicAnd ::= (and p p) | r
r ∈ functionCall ::= (x e*) | p
p ∈ primary ::= x | (e) | i | s
t ∈ stmt ::= (while (e){ (t*) e+}) | d
d ∈ enumerate ::= (enum x (case x)+) | g
g ∈ Switch ::= (switch x (case x (e))+ (default e)) | n
n ∈ (= v x e) | u
u ∈ print ::= (print e)
f ∈ function ::= (def v x (e*) {(f | (t | e))*} e) 
