#|-------------------------------------------------------------------------------------------------------------------
Funcion: Transicion
NATURALEZA: Pura
ESTRATEGIA: estructura condicional (implementada con COND)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
(load "  /   /   /quicklisp/setup.lisp")  ;tiene que ingresar la direccion donde se encuentra quicklisp
(ql:quickload "local-time")

(defun transicion (color-actual cambiar-a)
    (cond
        ((and (equal color-actual 'en-verde-intermitente) (equal cambiar-a 'cambiar-a-amarillo)) (list color-actual "CAMBIAR-A-AMARILLO" ))
        ((and (equal color-actual 'en-amarillo--intermitente) (equal cambiar-a 'cambiar-a-rojo)) (list color-actual "CAMBIAR-A-ROJO"))
        ((and (equal color-actual 'en-rojo-intermitente) (equal cambiar-a 'cambiar-a-verde)) (list color-actual "CAMBIAR-A-VERDE" ))
		((and (equal color-actual 'en-verde) (equal cambiar-a 'cambiar-a-verde-intermitente)) (list color-actual "CAMBIAR-A-VERDE-INTERMITENTE"))
		((and (equal color-actual 'en-amarillo) (equal cambiar-a 'cambiar-a-amarillo-intermitente)) (list color-actual "CAMBIAR-A-AMARILLO-INTERMITENTE"))
		((and (equal color-actual 'en-rojo) (equal cambiar-a 'cambiar-a-rojo-intermitente)) (list color-actual "CAMBIAR-A-ROJO-INTERMITENTE"))
        (t (list color-actual 'accion-por-defecto))
    )
)

(transicion 'en-verde-intermitente 'cambiar-a-amarillo)
(EN-VERDE-INTERMITENTE "CAMBIAR-A-AMARILLO")

(transicion 'en-verde-intermitente 'cambiar-a-rojo)
(EN-VERDE-INTERMITENTE ACCION-POR-DEFECTO)

(transicion 'en-verde 'cambiar-a-amarillo)
(EN-VERDE ACCION-POR-DEFECTO)

(transicion 'en-verde 'cambiar-a-amarillo-intermitente)
(EN-VERDE ACCION-POR-DEFECTO)

(transicion 'en-verde 'cambiar-a-amarillo-intermitente)
(EN-VERDE ACCION-POR-DEFECTO)

(transicion 'en-amarillo 'cambiar-a-amarillo-intermitente)
(EN-AMARILLO "CAMBIAR-A-AMARILLO-INTERMITENTE")


#|-------------------------------------------------------------------------------------------------------------------
Funcion: timer
Naturaleza: Pura
Estrategia: Simple (implementada con MOD y COND)
Impacto En Memoria: No Destructiva, no realiza cambios
-------------------------------------------------------------------------------------------------------------------|#

(defun timer (timestap) ;cuando tenes un semaforo, al terminar el tiempo de rojo, no se le suma +3 de intermitencia al rojo
									 ;la intermitencia empieza a cambiar en los ultimos 3 segundos de cada color
        (cond
            ((<= 0 (mod timestap 225) 89) 'en-rojo )
            ((<= 90 (mod timestap 225) 92) 'en-rojo-intermitente )
            ((<= 93 (mod timestap 225) 209) 'en-verde )
            ((<= 210 (mod timestap 225) 212) 'en-verde-intermitente )
            ((<= 213 (mod timestap 225) 215) 'en-amarillo )
            (t 'en-amarillo-intermitente) 
    )
)
;;;;CAMBIAR CASOS DE PRUEBA
(print(timer 526))
;EN-VERDE

(print(timer 2869))
;EN-ROJO

(print(timer 2895))
;EN-ROJO-INTERMITENTE

(print(timer 1))
;EN-ROJO

(print(timer 213))
;EN-AMARILLO-INTERMITENTE

(print(timer 212))
;EN-AMARILLO

(print(timer 217))
;EN-ROJO


#|-------------------------------------------------------------------------------------------------------------------
Funcion: LogginLights
Naturaleza: Impura (por FORMAT T escribe en pantalla segun los datos de entrada)
Estrategia: Simple (implementada con FORMAT )
Impacto En Memoria: No Destructiva, no realiza cambios
-------------------------------------------------------------------------------------------------------------------|#
;correcion: se automatizo el tiempo para que sea calculado directamente dentro de esta funcion, en vez de un tiempo que puede estar
;desactualizado, restamos al tiempo el cual esta dado desde 1970 unos 70 años para que se sicronicen correctamente. 

(defun logginLights (color-actual cambio-color unixtemp)
    (format t "~% Tiempo ~A la luz ah cambiado de color ~A a ~A~%" 
		(local-time:format-timestring nil (local-time:unix-to-timestamp unixtemp) :format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
			(car (transicion color-actual cambio-color)) (caddr (transicion color-actual cambio-color))
) )

;TRANSICION
(logginLights 'en-verde 'cambiar-a-amarillo 1781556453)
;Tiempo 2026-06-15 17:47:33 la luz ah cambiado de color EN-VERDE a CAMBIAR-A-AMARILLO
;NIL

(logginLights 'en-verde 'cambiar-a-rojo 1781556453)
;Tiempo 2026-06-15 17:47:33 la luz ah cambiado de color EN-VERDE a NIL ;caso de error
;NIL

(logginLights 'en-rojo 'cambiar-a-verde 1781556492)
;Tiempo 2026-06-15 17:48:12 la luz ah cambiado de color EN-ROJO a CAMBIAR-A-VERDE
;NIL

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: duracion-ciclo
NATURALEZA: Pura
ESTRATEGIA: secuancial (no recursiva, no predicado, no utiliza funciones de orden superior)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
; requerimiento 4.a 

(defun duracion-ciclo (rojo verde amarillo)
    (+ rojo verde amarillo 9) ;9 de intermitencia
)
;;;;CAMBIAR CASOS DE PRUEBA
;ciclo pedido (con iteracion 2 extension 1)
(print(duracion-ciclo 90 120 6))
;216

;otro ciclo
(print(duracion-ciclo 40 60 5))
;105

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: recomendacion-ciclo
NATURALEZA: Pura
ESTRATEGIA: condicional simple (condicional IF)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
; requerimiento 4.b

(defun recomendacion-ciclo (duracion)
    (if (and (>= duracion 35) (<= duracion 150))
		"ciclo optimo"
		"ciclo no optimo"
    )
)

(print (recomendacion-ciclo (duracion-ciclo 90 120 6 )))
;ciclo no optimo
(print (recomendacion-ciclo (duracion-ciclo 40 60 6 )))
;ciclo optimo
(print (recomendacion-ciclo (duracion-ciclo 7 12 5 )))
;ciclo no optimo

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: ciclos-por-tiempo
NATURALEZA: Impura (escribe en pantalla un resultado dependiendo de los minutos de entrada)
ESTRATEGIA: estructura secuencial (no presenta recursion en su implementacion)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
(defun ciclos-por-tiempo (minutos)
    (print "La cantidad de ciclos es de:")
    (print (truncate (/ (* minutos 60) 225)));truncate toma el resultado de una operacion y elimina el decimal, si el resultado es 28.9, quedaria 28
)
;caso pedido
(print(ciclos-por-tiempo 15))
;4

;otro caso
(print(ciclos-por-tiempo 70))
;18

#|-------------------------------------------------------------------------------------------------------------------   
FUNCION AUXILIAR: CalcularRestoIni
NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
-------------------------------------------------------------------------------------------------------------------|#

(defun calcularRestoIni (restoIni)
	(cond 						  ;(89 92 212 215 221 224)
        ((<= 0 restoIni 89) (list (- 89 restoIni) 3 120 3 6 3)) ;(- 89 restoIni) --> indica lo consumido por rojo
        ((<= 89 restoIni 92) (list 0 (- 92 restoIni) 120 3 6 3)) ;(- 92 restoIni) --> indica lo consumido por el rojo-intermitente
		((<= 93 restoIni 212) (list 0 0 (- 212 restoIni) 3 6 3)) ;(- 212 restoIni) --> indica lo consumido por verde
        ((<= 213 restoIni 215) (list 0 0 0 (- 215 restoIni) 6 3)) ;(- 215 restoIni) --> indica lo consumido por verde-intermitente
		((<= 216 restoIni 221) (list 0 0 0 0 (- 221 restoIni) 3)) ;(- 221 restoIni) --> indica lo consumido por amarillo
        (t (list 0 0 0 0 0 (- 224 restoIni))) ;(-224 restoIni) --> indica lo consumido por amarillo-intermitente
    )
)
;;;REHACER CASOS DE PRUEBA
(calcularRestoIni 5555) ;caso imposible con error
;NIL
	  
(calcularRestoIni 210)
;(0 0 0 0 2 3)

(calcularRestoIni 1)
;(85 3 117 3 3 3)

(calcularRestoIni 0) ;otro caso imposible, se verifica en su llamada si fuera 0
;(86 3 117 3 3 3)
	  
(calcularRestoIni 9)
;(77 3 117 3 3 3)
	  
#|-------------------------------------------------------------------------------------------------------------------
FUNCION AUXILIAR: CalcularRestoFin
NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
-------------------------------------------------------------------------------------------------------------------|#

(defun calcularRestoFin (restoFin)
	(cond  							;(89 92 212 215 221 224)
        ;; Al final de la hora es al revés: calculamos cuánto se consumió desde el inicio de ese último ciclo incompleto hasta llegar al segundo 'restoFin'
        ((<= 0 restoFin 89) (list restoFin 0 0 0 0 0)) ;(- 89 restoFin) --> indica lo consumido por rojo
        ((<= 90 restoFin 92) (list 90 (- restoFin  90) 0 0 0 0)) ;(- restoFin 90) --> indica lo consumido por el rojo-intermitente
		((<= 93 restoFin 212) (list 90 3 (- restoFin  93) 0 0 0)) ;(- restoFin  93) --> indica lo consumido por verde
        ((<= 213 restoFin 215) (list 90 3 120 (- restoFin  213) 0 0)) ;(- restoFin  213) --> indica lo consumido por verde-intermitente
		((<= 210 restoFin 212) (list 90 3 120 3 (- restoFin  210) 0)) ;(- restoFin  210) --> indica lo consumido por amarillo
        (t (list 90 3 120 3 6 (- restoFin  213))) ;(-  restoFin  213) --> indica lo consumido por amarillo-intermitente
    )
)
;;;CAMBIAR CASOS DE PRUEBA
(calcularRestoFin 5555) ;mismo caso imposible
;NIL

(calcularRestoFin 203)
;(87 3 113 0 0 0)

(calcularRestoFin 89)
;(87 2 0 0 0 0)

(calcularRestoFin 87)
;(87 0 0 0 0 0)
	  
(calcularRestoFin 120)
;(87 3 30 0 0 0)

(calcularRestoFin 0) ;caso imposible
;(0 0 0 0 0 0)
#|-------------------------------------------------------------------------------------------------------------------
FUNCION AUXILIAR: calcularPorcentajes
NATURALEZA: Pura (devuelve una lista con los porcentajes de cada estado del semáforo)
ESTRATEGIA: Secuencial (implementamos mediante operaciones aritméticas y la construcción de listas)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
;como ahora el ciclo es de 225, en 3600 entran 16 ciclos exactos, lo que se sabe es que 15 entran si o si, y hay que determinar que paso con el ultimo,
;si es que esta completo, o solo entro una parte
(defun calcularPorcentajes (ListaIni ListaFin)
	(list 
        (float (/ (* (+ 1350 (nth 0 ListaFin) (nth 0 ListaIni)) 100) 3600)) ;rojo
	    (float (/ (* (+ 45 (nth 1 ListaFin) (nth 1 ListaIni)) 100) 3600)) ;rojo-intermitente
	    (float (/ (* (+ 1800 (nth 2 ListaFin) (nth 2 ListaIni)) 100)3600)) ;verde
        (float (/ (* (+ 45 (nth 3 ListaFin) (nth 3 ListaIni)) 100) 3600)) ;verde-intermitente
        (float (/ (* (+ 90 (nth 4 ListaFin) (nth 4 ListaIni)) 100)3600)) ;amarillo
        (float (/ (* (+ 45 (nth 5 ListaFin) (nth 5 ListaIni)) 100) 3600)) ;amarillo-intermitente
    )
)


#|-------------------------------------------------------------------------------------------------------------------
FUNCION: distribucionTemp
NATURALEZA: impura (escribe en pantalla y en un archivo)
ESTRATEGIA: secuencial, condicional doble (implementada con if, format y nth)
IMPACTO: no destructiva
-------------------------------------------------------------------------------------------------------------------|#
;actualizado
(defun distribucionTemp (unix)
	(if (zerop (mod unix 225)) "| 40.0% rojo| 1.33% rojo-intermitente | 53.33% verde| 1.33% verde-intermitente| 2.66% amarillo| 1.33% amarillo intermitente|"
	    (format nil "~%| ~A% rojo| ~A% rojo-intermitente| ~A% verde | ~A% verde-intermitente| ~A% amarillo| ~A% amarillo-intermitente|"
	    (nth 0 (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
		(nth 1 (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
	    (nth 2 (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
        (nth 3 (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
        (nth 4 (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225))))
        (nth 5 (calcularPorcentajes (calcularRestoIni (mod unix 225)) (calcularRestoFin (mod (- 3600 (- 225 (mod unix 225))) 225)))))
    )					
)

;;;;;CAMBIAR CASOS DE PRUEBA
;lo cambiamos a nth ya que utilizar car/cdr duplicaba o utilizaba algunos valores erroneos

(print (distribuciontemp 3600))
;| 40.666668% rojo| 1.3333334% rojo-intermitente| 53.72222% verde | 1.4166666% verde-intermitente| 1.4166666% amarillo| 1.4166666% amarillo-intermitente|

;Extension 2, sistema de datos

;Actualizado

#|-------------------------------------------------------------------------------------------------------------------
FUNCION: crear-informe
NATURALEZA: inpura (escribe en el archivo)
ESTRATEGIA: Utiliza una estructura condicional
IMPACTO: no destructiva
-------------------------------------------------------------------------------------------------------------------|#
(defun crear-informe ()
  (unless (probe-file "informe-ejecucion-semaforo.txt") ;probe-file comprueba si existe el archivo.txt, si existe, da verdadero, sino falso, y un unless es como if,
    ;pero solo ejecuta si la condicion es falsa
    (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output)
      (format stream "Informe de Ejecución del Sistema Semaforico~%")
      )
    )
  )

#|-------------------------------------------------------------------------------------------------------------------
FUNCION: cerrar-informe
NATURALEZA: inpura (escribe en el archivo)
ESTRATEGIA: finaliza el archivo
IMPACTO: no destructiva
-------------------------------------------------------------------------------------------------------------------|#
(defun cerrar-informe()
   (with-open-file (stream "informe-ejecucion-semaforo.txt":direction :output :if-exists :append)
       (format stream "~% --- Fin del Informe ---~%")
   )
 )

#|-------------------------------------------------------------------------------------------------------------------
FUNCION: informe
NATURALEZA: impura (escribe en pantalla y en un archivo)
ESTRATEGIA: Secuencial
IMPACTO: no destructiva
-------------------------------------------------------------------------------------------------------------------|#

(defun informe (color-actual cambio-color unixtemp)
(crear-informe)
 (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :append :if-does-not-exist :create)
   (format stream "~%=========================================~%")
   (format stream "~A transicion: ~A --> ~A~%"
   (local-time:format-timestring nil (local-time:unix-to-timestamp unixtemp) :format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
   (car (transicion color-actual cambio-color)) (caddr (transicion color-actual cambio-color)) )
   )
	;pasandole loggin para ver en pantalla
   (logginLights color-actual cambio-color unixtemp)
)

(informe 'en-verde 'cambiar-a-amarillo 1742163000)
(informe 'en-verde 'cambiar-a-rojo 1742163000)
(informe 'en-amarillo 'cambiar-a-rojo 1718484493)
                                   
;cierra el informe
(cerrar-informe)
