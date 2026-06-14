/* funcion: transicion */
def transicion(colorActual: String, colorACambiar: String) = { // declare la funcion, como parametro las variables strings
  (colorActual, colorACambiar) match { // funciona como un cond, donde los 2 parametros se comparan en cada caso
    case ("rojo", "verde") => (println(s"$colorActual rojoIntermitente cambia a $colorACambiar"))
    // caso1. => indica que hacia al lado donde apunta es el resultado, en este caso el mensaje a imprimir
    case ("verde", "amarillo") => (println(s"$colorActual verdeIntermitente cambia a $colorACambiar"))
    // caso 2. => indica que hacia al lado donde apunta es el resultado, en este caso el mensaje a imprimir
    case ("amarillo", "rojo") => (println( s"$colorActual amarilloIntermitente cambia a $colorACambiar"))
    // caso 3. => indica que hacia al lado donde apunta es el resultado, en este caso el mensaje a imprimir
  }
}
//no esta terminado todavia no devuelve de manera exacta(no devuelve una lista)

//caso probado
//rojo rojoIntermitente cambia a verde
transicion("rojo", "verde")
