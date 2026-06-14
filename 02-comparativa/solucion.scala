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
    // caso 4. => indica que si no se cumple con las condiciones anteriores, este devuelve el color actual y accion por defecto
  }
}

/*casos probados
transicion("en-rojo", "amarillo")
List(en-rojo, accion-por-defeto)

transicion("en-rojo", "verde")
List(en-rojo, cambiar-a-verde )

transicion("en-verde", "amarillo")
List(en-verde, cambiar-a-amarillo)

transicion("amarillo", "rojo")
List(en-amarillo, cambiar-a-rojo)
*/

/*funcion timer*/
def timer() = {
  
}
