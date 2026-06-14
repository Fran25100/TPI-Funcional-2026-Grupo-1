/* funcion: transicion */
def transicion(colorActual: String, colorACambiar: String) = { // declare la funcion, como parametro las variables strings
  (colorActual, colorACambiar) match { // funciona como un cond, donde los 2 parametros se comparan en cada caso
    case ("rojo", "verde") => (List (colorActual ,colorActual,"Intermitente cambia-a-",colorACambiar))
    // caso1. => indica que hacia al lado donde apunta es el resultado, en este caso el mensaje a imprimir
    case ("verde", "amarillo") => (List (colorActual ,colorActual,"Intermitente cambia-a-",colorACambiar))
    // caso 2. => indica que hacia al lado donde apunta es el resultado, en este caso el mensaje a imprimir
    case ("amarillo", "rojo") => (List (colorActual ,colorActual,"Intermitente cambia-a-",colorACambiar))
    // caso 3. => indica que hacia al lado donde apunta es el resultado, en este caso el mensaje a imprimir
    case _ => (List(colorActual, "accion-por-defeto"))
    // caso 4. => si no es como los otros 3 casos devuelve una lista con el colorActual y el string "accion-por-defecto"
  }
}


/*casos probados
transicion("rojo", "amarillo")
List(rojo, accion-por-defeto)

transicion("rojo", "verde")
List(rojo, rojo,intermitente color-a-,verde )

*/
