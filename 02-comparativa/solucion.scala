/* funcion: transicion */
def transicion(colorActual: String, colorACambiar: String): List[String] = { // declare la funcion, como parametro las variables strings
  (colorActual, colorACambiar) match { // funciona como un cond, donde los 2 parametros se comparan en cada caso
    case ("en-rojo", "verde") => List(colorActual, "cambiar-a-verde")
    // caso1. => indica hacia que lado apunta el resultado, en este caso el mensaje a imprimir
    case ("en-verde", "amarillo") => List(colorActual, "cambiar-a-amarillo")
    // caso 2. => indica hacia que lado apunta el resultado, en este caso el mensaje a imprimir
    case ("en-amarillo", "rojo") => List(colorActual, "cambiar-a-rojo")
    // caso 3. => indica hacia que lado apunta el resultado, en este caso el mensaje a imprimir
    case _ => List(colorActual, "accion-por-defecto")
  }
}

/*casos probados
transicion("rojo", "amarillo")
List(rojo, accion-por-defeto)

transicion("rojo", "verde")
List(rojo, color-a-verde )
*/
