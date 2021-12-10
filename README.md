# dlp_2021

## Estudio estudio gas gas gas gas

Obligatorios (5 ptos +1 si ingles):

* (Listo) Reconocimiento de expresiones multi-línea
* (Listo) Incorporación de un punto fijo interno para declarar definiciones recursivas directas
* (Listo) Incorporación de un contexto en la terminal -> contexto funcional
* (SemiListo) Incorporación del tipo string: >> "a"^"a"^"a";; -> syntax error (ok)
* (Listo) Incorporación de pares.
* Memoria

Opcionales:

* (0.75) Reconocimiento de expresiones desde fichero
* (1.50) pretty-printer
* (0.25) modo debug
* (1.50) ındicesdeDeBruijn
* (1.00) registros
* (1.00) listas
* (2.00) subtipado

DUDAS:

* en la gramática hector metio las tuplas en appterm, estan bien mi senor??
* en tmproj int / termino que sea un entero??
* conflicto con STRING y cosas como LAMBDA o L

TODO

* Esciribir en el fichero examples.txt -> ejemplos adicionales que involucren doble recursividad a partir de la suma (Fibonacci)

Dudas:

* : Nat = ({1 , 2}).1 -> esto no lo reconoce la gramática y realmente no cunde que no lo haga porque un (id({1,2})).1 no lo reconocería tampoco -> id = (L x : Nat.{x,x});; (id 1).1;;
* contexto funcional ->  basicamente el problema es cuando tienes una abstraccion sin evaluar, como no se va a aplicar la var al contexto y deberia fijarse al valor de la var al contexto en ese momento de ejecución vamos a tener que comernos luego el término para fijar lo libre.
* no me funciona algo del rollo L x.x -> hay que especificarle el tipo wtf
* que es el __s__ token -> en funcs del lexer/parser me pierdo algo

MEMORIA

String Cualquier cosa que no sea un terminador, op concat como en ocaml
Tuplas (x,y) el tipo se especifica con *
