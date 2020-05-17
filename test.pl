call_closure(Retval1, Retval2) :- copy_term(Retval1, (Retval2 :- Retval3)), call(Retval3).
switch(Retval4, [Retval5:Retval6|Retval7]) :- ((Retval4=Retval5 ; Retval5 == "default") -> call(Retval6) ; switch(Retval4, Retval7)).
y(jay).y(scott).
foo(I, Retval8) :- Bar = ((J,  Retval10 ), Retval10 is I+J), Retval8 is Bar.
foo(2, Retval11), T = Retval11.
Peep = call_closure(T, 3).
switch(y(jay), [ y(jay) : call_closure(T, 4), y(scott) : Peep+6, "default" : Peep-2 ]).
