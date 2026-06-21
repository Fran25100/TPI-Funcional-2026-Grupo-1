/* funcion: transicion */

//correcto:
def transicion(colorActual: String, colorACambiar: String): List[String] = { // declare la funcion, como parametro las variables strings
  (colorActual, colorACambiar) match { // funciona como un cond, donde los 2 parametros se comparan en cada caso
    case ("en-rojo", "en-rojo-intermitente") => List(colorActual, "cambiar-a-rojo-intermitente")
    // caso1. => compara los parametros string, cumple con el caso muestra una lista ("en-rojo" "en-rojo-intermitente")
    case ("en-rojo-intermitente", "en-verde") => List(colorActual, "cambiar-a-verde")
    // caso 2. => compara los parametros string, cumple con el caso muestra una lista ("en-rojo-intermitente") ("en-verde")
    case ("en-verde", "en-verde-intermitente") => List(colorActual, "cambiar-a-verde-intermitente")
    // caso 3. => compara los parametros string, cumple con el caso muestra una lista ("en-verde") ("en-verde-intermitente")
    case ("en-verde-intermitente", "en-amarillo") => List(colorActual, "cambiar-a-amarillo")
    // caso 4. => compara los parametros string, cumple con el caso muestra una lista ("en-verde-intermitente") ("en-amarillo")
    case ("en-amarillo", "en-amarillo-intermitente") => List(colorActual, "cambiar-a-amairllo-intermitente")
    // caso 5. => compara los parametros string, cumple con el caso muestra una lista ("en-amarillo") ("en-amarillo-intermitente")
    case ("en-amarillo-intermitente", "en-rojo") => List(colorActual, "cambiar-a-rojo")
    // caso 6. => compara los parametros string, cumple con el caso muestra una lista ("en-verde") ("en-verde-intermitente")
    case _ => List(colorActual, "accion-por-defecto")
    // caso 7. => indica que si no se cumple con las condiciones anteriores, este devuelve el color actual y accion por defecto
  }
}
// actualizado con intermitentes

/* casos probados */
transicion("en-rojo", "en-rojo-intermitente")
//List(en-rojo, cambia-a-rojo-intermitente)

transicion("en-rojo-intermitente", "en-verde")
//List(en-rojo-intermitente, "cambiar-a-verde")

transicion("en-verde", "en-verde-intermitente")
//List(en-verde, "en-verde-intermitente")

transicion("en-verde-intermitente", "en-amarillo")
//List(en-verde-intermitente, "cambiar-a-amarillo")

transicion("en-amarillo", "en-amarillo-intermitente")
//List(en-amarillo, cambiar-a-amarillo-intermitente)

transicion("en-amarillo-intermitente", "en-rojo")
//List(en-amarillo-intermitente , "cambiar-a-rojo")

transicion("en-rojo", "en-verde")
//List(en-rojo, accion-por-defeto)


/*funcion timer*/

//correcto:      
def timer(timestap: Float): String = {//funcion timer que recibe el parametro timestap
  (timestap % 225.0f) match {//declaro el match con el parametro timestap
    // Aplicamos el módulo 225.0f para limpiar los ciclos completos, siempre va a valer entre 0 y 224
    case time if time >= 0.0f && time <= 89.0f => "en-rojo"
    // caso 1. => comparamos si esta dentro del rango para ser rojo
    case time if time >= 90.0f && time <= 92.0f => "en-rojo-intermitente"
    // caso 2. => comparamos si esta dentro del rango para ser rojo-intermitente
    case time if time >= 93.0f && time <= 212.0f => "en-verde"
    // caso 3. => comparamos si esta dentro del rango para ser verde
    case time if time >= 213.0f && time <= 215.0f => "en-verde-intermitente"
    // caso 4. => comparamos si esta dentro del rango para ser verde-intermitente
    case time if time >= 216.0f && time <= 221.0f => "en-amarillo"
    // caso 5. => comparamos si esta dentro del rango para ser amarillo
    case time if time >= 222.0f && time <= 224.0f => "en-amarillo-intermitente"
    // caso 6. => comparamos si esta dentro del rango para ser amarillo-intermitente
  }
}
//actualizado con intermitentes

/*casos probados
timer(1000)
//en-verde

timer(0)
//en-rojo

timer(27)
//en-rojo

timer(89)
//en-rojo

timer(90)
//en-rojo-intermitente

timer(91)
//en-rojo-intermitente

timer(92)
//en-rojo-intermitente

timer(93)
//en-verde

timer(170)
//en-verde

timer(212)
//en-verde

timer(213)
//en-verde-intermitente

timer(214)
//en-verde-intermitente

timer(215)
//en-verde-intermitente

timer(216)
//en-amarillo

timer(219)
//en-amarillo

timer(221)
//en-amarillo

timer(222)
//en-amarillo-intermitente

timer(223)
//en-amarillo-intermitente

timer(224)
//en-amarillo-intermitente

timer(2000)
//en-verde

