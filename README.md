# dlp_2021

## Estudio estudio gas gas gas gas

Obligatorios (5 ptos +1 si ingles):

* Reconocimiento de expresiones multi-línea
* Incorporación de un punto fijo interno para declarar definiciones recursivas directas (jodido)
* Incorporación de un contexto en la terminal -> contexto funcional (jodido)
* Incorporación del tipo string
* Incorporación de pares: Incorporar a la gram., añadir TmPair en tipos y completar pattern-matching y reglas eval y tipado.
* Memoria

Opcionales:

* (0.75) Reconocimiento de expresiones desde fichero
* (1.50) pretty-printer
* (0.25) modo debug
* (1.50) ındicesdeDeBruijn
* (1.00) registros
* (1.00) listas
* (2.00) subtipado

Dudas:

* contexto funcional ->  basicamente el problema es cuando tienes una abstraccion sin evaluar, como no se va a aplicar la var al contexto y deberia fijarse al valor de la var al contexto en ese momento de ejecución vamos a tener que comernos luego el término para fijar lo libre.
* no me funciona algo del rollo L x.x -> hay que especificarle el tipo wtf
* que es el __s__ token -> en funcs del lexer/parser me pierdo algo
