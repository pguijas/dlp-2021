# dlp_2021

## Estudio estudio gas gas gas gas

Obligatorios (5 ptos +1 si ingles):

* (Listo) Reconocimiento de expresiones multi-línea
* (Listo) Incorporación de un punto fijo interno para declarar definiciones recursivas directas
* (Listo) Incorporación de un contexto en la terminal -> contexto funcional
* (Listo) Incorporación del tipo string: >> "a"^"a"^"a";; -> syntax error (ok)
* (Listo) Incorporación de pares.
* Memoria

Opcionales:

* (0.25) modo debug
* (1.00) registros -> pendiente de duda graña
* (1.00) listas -> cabeza ( head() o .hd ), cola( tail() o .tl) y es vacía -> (me confundí y hice la proyección tambien, pero queda para los registros)
* (2.00) subtipado

DUDAS:

* en tmproj string
* el casque de resistros y out of bounds casca en el eval no??
* construir las listas es un puto coñazo
* añadi una regla, las projs pueden ser: | atomicTerm DOT INTV ,| atomicTerm DOT STRINGV -> el control de que se escapa (2> tuplas) lo hacemos en tiempo de tipado, esta bien?
* el record puede ser vací? si no, pues me cargo esta regla | RBRACKET { [] }
* que pasa con claves repetidas?
* estoy haciendo de forma elegante la gramatica de registros? -> y esta en buen sitio (funcionar funciona), lo puse en atomicTy y atomicTerm!
* Que pasa con el orden, si se cambia el orden de ocurrencia de los reg ya la liamos

Datos:

* no usar palabra reservada list, he pecado :()

TODO

* Esciribir en el fichero examples.txt -> ejemplos adicionales que involucren doble recursividad a partir de la suma (Fibonacci)
* revisar todo el código y pulirlo (antes de la ultima clase), mucho comentario y mucha mierda
* hacer el map de listas
* Revisar los parentesis en los prints
* Debuguear de alguna forma bien todo lo de listas porque es mucho código con bucles sin revisar -> buscar cassos de casque
* Mirar de cambiar todos los letrecs de TmREcord por 2 reglas en el pattern matching principal
* reconocer type de reg
* Revisar que todos los types funcionen con unas funcionces lambda

MEMORIA

String Cualquier cosa que no sea un terminador, op concat como en ocaml
Tuplas (x,y) el tipo se especifica con *
