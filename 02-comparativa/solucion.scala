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
def timer(timestap: Float): String = {//funcion timer que recibe el parametro timestap
  (timestap % 215.0f) match {//declaro el match con el parametro timestap
    // Aplicamos el módulo 216.0f para limpiar los ciclos completos, siempre va a valer entre 0 y 215(como si fuera de 1-216).
    case time if time >= 0.0f && time <= 86f.0 => "en-rojo"
    // caso 1. => comparamos si esta dentro del rango para ser rojo
    case time if time >= 86.0f && time <= 89.0f => "en-rojo-intermitente"
    // caso 2. => comparamos si esta dentro del rango para ser rojo-intermitente
    case time if time >= 89.0f && time <= 206.0f => "en-verde"
    // caso 3. => comparamos si esta dentro del rango para ser verde
    case time if time >= 207.0f && time <= 209.0f => "en-verde-intermitente"
    // caso 4. => comparamos si esta dentro del rango para ser verde-intermitente
    case time if time >= 210.0f && time <= 212.0f => "en-amarillo"
    // caso 5. => comparamos si esta dentro del rango para ser amarillo
    case time if time >= 213.0f && time <= 215.0f => "en-amarillo-intermitente"
    // caso 6. => comparamos si esta dentro del rango para ser amarillo-intermitente
  }
}
/*
//casos probados
timer(1000)
en-verde

timer(0)
en-rojo

timer(27)
en-rojo

timer(86)
en-rojo

timer(87)
en-rojo-intermitente

timer(88)
en-rojo-intermitente

timer(89)
en-rojo-intermitente

timer(90)
en-verde

timer(170)
en-verde

timer(206)
en-verde

timer(207)
en-verde-intermitente

timer(208)
en-verde-intermitente

timer(209)
en-verde-intermitente

timer(210)
en-amarillo

timer(211)
en-amarillo

timer(212)
en-amarillo

timer(213)
en-amarillo-intermitente

timer(214)
en-amarillo-intermitente

timer(215)
en-amarillo-intermitente

timer(2000)
en-rojo
*/
