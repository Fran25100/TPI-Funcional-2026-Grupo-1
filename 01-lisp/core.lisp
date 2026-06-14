#|-------------------------------------------------------------------------------------------------------------------
Funcion: Transicion
NATURALEZA: Pura
ESTRATEGIA: estructura condicional (implementada con COND)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
(load "  /"   "/"   "/quicklisp/setup.lisp")  ;tiene que ingresar la direccion donde se encuentra quicklisp
(ql:quickload "local-time")

(defun transicion (color-actual cambiar-a)
    (cond
        ((and (equal color-actual 'en-verde) (equal cambiar-a 'cambiar-a-amarillo)) (list color-actual 'verde-intermitente "CAMBIAR-A-AMARILLO" ))
        ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'cambiar-a-rojo)) (list color-actual 'amarillo-intermitente "CAMBIAR-A-ROJO"))
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'cambiar-a-verde)) (list color-actual 'rojo-intermitente "CAMBIAR-A-VERDE" ))
        (t (list color-actual 'accion-por-defecto ))
    )
)

(print(transicion 'en-verde 'cambiar-a-amarillo))
;(EN-VERDE VERDE-INTERMITENTE "CAMBIAR-A-AMARILLO")

(print(transicion 'en-verde 'cambiar-a-rojo))
;(EN-VERDE ACCION-POR-DEFECTO)

(print(transicion 'en-amarillo 'cambiar-a-rojo))
;(EN-AMARILLO AMARILLO-INTERMITENTE "CAMBIAR-A-ROJO")



#|-------------------------------------------------------------------------------------------------------------------
Funcion: timer
Naturaleza: Pura
Estrategia: Simple (implementada con MOD y COND)
Impacto En Memoria: No Destructiva, no realiza cambios
-------------------------------------------------------------------------------------------------------------------|#

(defun timer (timestap) ;cuando tenes un semaforo, al terminar el tiempo de rojo, no se le suma +3 de intermitencia al rojo
									 ;la intermitencia empieza a cambiar en los ultimos 3 segundos de cada color
        (cond
            ((<= 0 (mod timestap 216) 86) 'en-rojo )
            ((<= 87 (mod timestap 216) 89) 'en-rojo-intermitente )
            ((<= 90 (mod timestap 216) 206) 'en-verde )
            ((<= 207 (mod timestap 216) 209) 'en-verde-intermitente )
            ((<= 210 (mod timestap 216) 212) 'en-amarillo )
            (t 'en-amarillo-intermitente) 
    )
)

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
Naturaleza: Impura (por FORMAT T y GET-UNIVERSAL-TIME, escribe en pantalla segun los datos de entrada)
Estrategia: Simple (implementada con FORMAT y GET-UNIVERSAL-TIME)
Impacto En Memoria: No Destructiva, no realiza cambios
-------------------------------------------------------------------------------------------------------------------|#
;correcion: se automatizo el tiempo para que sea calculado directamente dentro de esta funcion, en vez de un tiempo que puede estar
;desactualizado, restamos al tiempo el cual esta dado desde 1970 unos 70 años para que se sicronicen correctamente. 

(defun logginLights (color-actual cambio-color)
    (format nil "Tiempo ~A: la luz ah cambiado de color ~A a ~A" 
		(local-time:format-timestring nil (local-time:now):format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))
			(car (transicion color-actual cambio-color)) (caddr (transicion color-actual cambio-color))
) )

(logginLights 'en-rojo 'en-rojo-intermitente)
;Tiempo 1781220382: la luz ah cambiado de color EN-ROJO a EN-ROJO-INTERMITENTE

(logginLights 'en-rojo-intermitente 'en-verde)
;Tiempo 1781220197: la luz ah cambiado de color EN-ROJO-INTERMITENTE a EN-VERDE

(logginLights 'en-verde 'en-verde-intermitente)
;Tiempo 1781220439: la luz ah cambiado de color EN-VERDE a EN-VERDE-INTERMITENTE

(logginLights 'en-verde-intermitente 'en-amarillo)
;Tiempo 1781220476: la luz ah cambiado de color EN-VERDE-INTERMITENTE a EN-AMARILLO

(logginLights 'en-amarillo 'en-amarillo-intermitente)
;Tiempo 1781220500: la luz ah cambiado de color EN-AMARILLO a EN-AMARILLO-INTERMITENTE

(logginLights 'en-amarillo-intermitente 'en-rojo)
;Tiempo 1781220532: la luz ah cambiado de color EN-AMARILLO-INTERMITENTE a EN-ROJO

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: duracion-ciclo
NATURALEZA: Pura
ESTRATEGIA: Simple (no recursiva, no predicado, no utiliza funciones de orden superior)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
; requerimiento 4.a 

(defun duracion-ciclo (rojo verde amarillo intermitente )
    (+ rojo verde amarillo (* intermitente 3))
)

;ciclo pedido (con iteracion 2 extension 1)
(print(duracion-ciclo 87 117 3 3))
;216

;otro ciclo
(print(duracion-ciclo 40 60 5 2))
;111

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: recomendacion-ciclo
NATURALEZA: Pura
ESTRATEGIA: alternativa doble (condicional IF)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
; requerimiento 4.b

(defun recomendacion-ciclo (duracion)
    (if (and (>= duracion 35) (<= duracion 150)); preferi no ser tan especifico y cambiarlo a un if
		"ciclo optimo"
		"ciclo no optimo"
    )
)

(print (recomendacion-ciclo (duracion-ciclo 90 120 6 3)))
;ciclo no optimo
(print (recomendacion-ciclo (duracion-ciclo 40 60 6 3)))
;ciclo optimo
(print (recomendacion-ciclo (duracion-ciclo 7 12 5 3)))
;ciclo no optimo

#|-------------------------------------------------------------------------------------------------------------------
FUNCIÓN: ciclos-por-tiempo
NATURALEZA: Impura (escribe en pantalla un resultado dependiendo de los minutos de entrada)
ESTRATEGIA: estructura secuencial (no presenta recursion en su implementacion)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#
(defun ciclos-por-tiempo(minutos)
    (print "La cantidad de ciclos es de:")
    (print (truncate (/ (* minutos 60) 216)));truncate toma el resultado de una operacion y elimina el decimal, si el resultado es 28.9, quedaria 28
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

#|(defun calcularRestoIni (restoIni)
	(cond 
        ((<= 0 restoIni 86) (list (- 86 restoIni) 3 117 3 3 3)) ;(- 86 restoIni) --> indica lo consumido por rojo
        ((<= 87 restoIni 90) (list 0 (- 90 restoIni) 117 3 3 3)) ;(- 90 restoIni) --> indica lo consumido por el rojo-intermitente
		((<= 91 restoIni 206) (list 0 0 (- 206 restoIni) 3 3 3)) ;(- 206 restoIni) --> indica lo consumido por verde
        ((<= 207 restoIni 209) (list 0 0 0 (- 209 restoIni) 3 3)) ;(- 209 restoIni) --> indica lo consumido por verde-intermitente
		((<= 209 restoIni 212) (list 0 0 0 0 (- 212 restoIni) 3)) ;(- 212 restoIni) --> indica lo consumido por amarillo
        ((<= 212 restoIni 215) (list 0 0 0 0 0 (- 215 restoIni))) ;(-215 restoIni) --> indica lo consumido por amarillo-intermitente
    )
)|#
(defun calcularRestoIni (restoIni)
					(cond ((<= 0 restoIni 89) (list (- 90 restoIni) 120 6)) ;(-90 restoIni) --> indica lo consumido por rojo
					((<= 90 restoIni 209) (list 0 (- 216 restoIni 6) 6)) ;(- 216 restoIni 6) --> indica lo consumido por verde
					(t (list 0 0 (- 216 restoIni)))
)
)

#|-------------------------------------------------------------------------------------------------------------------
FUNCION AUXILIAR: CalcularRestoFin
NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
ESTRATEGIA: alternativa Multiple (cond)
IMPACTO: No destrutiva
-------------------------------------------------------------------------------------------------------------------|#

#|(defun calcularRestoFin (restoFin)
	(cond  
        ;; Al final de la hora es al revés: calculamos cuánto se consumió desde el inicio de ese último ciclo incompleto hasta llegar al segundo 'restoFin'
        ((<= 0 restoFin 86) (list restoFin 0 0 0 0 0)) ;(- 86 restoFin) --> indica lo consumido por rojo
        ((<= 87 restoFin 89) (list 87 (-  restoFin  87) 0 0 0 0)) ;(- 87 restoFin) --> indica lo consumido por el rojo-intermitente
		((<= 90 restoFin 206) (list 87 3 (-  restoFin  90) 0 0 0)) ;(- 90 restoFin) --> indica lo consumido por verde
        ((<= 207 restoFin 209) (list 87 3 117 (-  restoFin  207) 0 0)) ;(- 207 restoFin) --> indica lo consumido por verde-intermitente
		((<= 210 restoFin 212) (list 87 3 117 3 (-  restoFin  210) 0)) ;(- 210 restoFin) --> indica lo consumido por amarillo
        ((<= 213 restoFin 215) (list 87 3 117 3 3 (-  restoFin  213))) ;(- 213 restoFin) --> indica lo consumido por amarillo-intermitente
    )
)|#
(defun calcularRestoFin (restoFin)
				(cond ((<= 0 restoFin 89) (list restoFin 0 0)) ; restoFin --> indica lo consumido por rojo
					((<= 90 restoFin 209) (list 90 (- restoFin 89) 0)) ;(- restoFin 89) --> indica lo consumido por verde 0|----R-----|89/90|------V------|209/210|---A---|
					(t (list 90 120 (- restoFin 209)) ) ; (- restoFin 209) --> indica lo consumido por amarillo
)
)


#|-------------------------------------------------------------------------------------------------------------------
FUNCION AUXILIAR: calcularPorcentajes
NATURALEZA: Pura (devuelve una lista con los porcentajes de cada estado del semáforo)
ESTRATEGIA: Secuencial (implementamos mediante operaciones aritméticas y la construcción de listas)
IMPACTO: No destructiva
-------------------------------------------------------------------------------------------------------------------|#

#|
defun calcularPorcentajes (ListaIni ListaFin)
	(list 
        (float (/ (* (+ 1392 (nth 0 ListaFin) (nth 0 ListaIni)) 100) 3600)) ;rojo
	    (float (/ (* (+ 48 (nth 1 ListaFin) (nth 1 ListaIni)) 100) 3600)) ;rojo-intermitente
	    (float (/ (* (+ 1872 (nth 2 ListaFin) (nth 2 ListaIni)) 100)3600)) ;verde
        (float (/ (* (+ 48 (nth 3 ListaFin) (nth 3 ListaIni)) 100) 3600)) ;verde-intermitente
        (float (/ (* (+ 48 (nth 4 ListaFin) (nth 4 ListaIni)) 100)3600)) ;amarillo
        (float (/ (* (+ 48 (nth 5 ListaFin) (nth 5 ListaIni)) 100) 3600)) ;amarillo-intermitente
    )
)

)|#
(defun calcularPorcentajes (ListaIni ListaFin)
				(list (float(/(*(+ 1440 (car ListaFin) (car ListaIni))100) 3600)) ;rojo
				(float(/(*(+ 1920 (cadr ListaFin) (cadr ListaIni))100)3600)) ;verde
				(float(/(*(+ 96 (caddr ListaFin) (caddr ListaIni))100)3600)) ;amarillo
				
)
)


#| CASO FALLIDO:
(defun distribucionTemp (unix)
				(if (zerop (mod unix 216)) "| 55,5% verde| 41,6% rojo | 2,7% amarillo|" ------> el mod nos muestra
	donde estamos parados, si es = 0 son 16 verdes completos, 16 amarillos y 16,6 rojos. en porcentajes de tiempo serian:
	55,5% V, 41,6% R y 2,7% A.
				;((not(zerop (mod unix 216) 0)) 
				(format t "| ~A% rojo| ~A% amarillo | ~A% verde|"
						(car (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (mod unix 216)) 216) ) ))
			;entra al primer elemento (porcentaje del rojo) de la lista, calcularPorcentajes recibe 2 parametros
				;el 1er, devuelve una lista de los restos CONSUMIDOS al inicio extremo de la hora
				;el 2do, devuelve una lista con los restos CONSUMIDOS en el extremo final de la hora, el calculo "(mod (- 3600 (mod unix 216)) 216)"
						;es el resultado de: (- 3600 (mod unix 216) = "tiempo Acotado" que es lo que me queda del tiempo en la 2da hora (sacando el consumido
						;del resto inicial), y el mod externo nos da el extremo final de esa hora.
					(cadr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (mod unix 216)) 216) ) ))
			;entra al 2do elemento de la lista de los porcentajes (especificamente el amarillo) y hace el mismo proceso anterior
				(caddr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (mod unix 216)) 216) ) ))
			;entra al 3er elemento de la lista de los porcentajes (verde), sigue el mismo proceso
	)
)					
)|#
				
;CORRECTO
#|(defun distribucionTemp (unix)
				(if (zerop (mod unix 216)) "| 55,5% verde| 41,6% rojo | 2,7% amarillo|" ------> el mod nos muestra
	donde estamos parados, si es = 0 son 16 verdes completos, 16 amarillos y 16,6 rojos. en porcentajes de tiempo serian:
	55,5% V, 41,6% R y 2,7% A.
				
				;((not(zerop (mod unix 216) 0)) 
				(format t "| ~A% rojo| ~A% amarillo | ~A% verde|"
						(car (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
						;entra al primer elemento (porcentaje del rojo) de la lista, calcularPorcentajes recibe 2 parametros
				;el 1er, devuelve una lista de los restos CONSUMIDOS al inicio extremo de la hora
				;el 2do, devuelve una lista con los restos CONSUMIDOS en el extremo final de la hora, el calculo "(mod (- 3600 (- 216(mod unix 216))) 216)"
						;es el resultado de: (- 3600 (- 216(mod unix 216))) = "tiempo Acotado" que es lo que me queda del tiempo en la 2da hora (sacando el consumido
						;del resto inicial), y el mod externo nos da el extremo final de esa hora.
					(cadr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
					;entra al 2do elemento de la lista de los porcentajes (especificamente el amarillo) y hace el mismo proceso anterior
				(caddr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
				;entra al 3er elemento de la lista de los porcentajes (verde), sigue el mismo proceso
	)
)					
)|#
(defun distribucionTemp (unix)
				(if (zerop (mod unix 216)) "| 41,6% rojo | 55,5% verde | 2,7% amarillo|" #|------> el mod nos muestra
	donde estamos parados, si es = 0 son 16 verdes completos, 16 amarillos y 16,6 rojos. en porcentajes de tiempo serian:
	55,5% V, 41,6% R y 2,7% A.|#
				
				;((not(zerop (mod unix 216) 0)) 
				(format t "| ~A% rojo| ~A% verde | ~A% amarillo|"
						(car (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
					(cadr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
				(caddr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216(mod unix 216))) 216) ) ))
	)
)					
)
;--------------------------------------------------------------------------------------------------------------------------------------
#|(defun distribucionTemp (unix)
	(if (zerop (mod unix 216)) "| 40,28% rojo| 1,39% rojo-intermitente | 54,17% verde| 1,39% verde-intermitente| 1,39% amarillo| 1,39% amarillo intermitente|"
	    (format t "~%| ~A% rojo| ~A% rojo-intermitente| ~A% verde | ~A% verde-intermitente| ~A% amarillo| ~A% amarillo-intermitente|"
	    (car (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
		(cadr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
	    (caddr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
        (cadddr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
        (cadddr (cdr (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216)))))
        (car (last (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))))
    )					
)|#

(print (distribuciontemp 3600))

;actualizado
(defun distribucionTemp (unix)
	(if (zerop (mod unix 216)) "| 40,28% rojo| 1,39% rojo-intermitente | 54,17% verde| 1,39% verde-intermitente| 1,39% amarillo| 1,39% amarillo intermitente|"
	    (format t "~%| ~A% rojo| ~A% rojo-intermitente| ~A% verde | ~A% verde-intermitente| ~A% amarillo| ~A% amarillo-intermitente|"
	    (nth 0 (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
		(nth 1 (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
	    (nth 2 (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
        (nth 3 (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
        (nth 4 (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216))))
        (nth 5 (calcularPorcentajes (calcularRestoIni (mod unix 216)) (calcularRestoFin (mod (- 3600 (- 216 (mod unix 216))) 216)))))
    )					
)
;lo cambiamos a nth ya que utilizar car/cdr duplicaba o utilizaba algunos valores erroneos

;Extension 2, sistema de datos

#|-------------------------------------------------------------------------------------------------------------------
FUNCION: informe
NATURALEZA: impura (escribe en pantalla y en un archivo)
ESTRATEGIA: Secuencial
IMPACTO: Destructiva
-------------------------------------------------------------------------------------------------------------------|#
#|
(defun informe (datos)
 (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output)
   (format stream "Informe de Ejecución del Sistema Semafórico~%")
   (format stream "=========================================~%")

   (mapcar #'(lambda (x) (format stream "~A~%" x)) datos)

   (format stream "~% --- Fin del Informe ---")
   )
 )

(informe (list (logginLights 'en-rojo 'en-rojo-intermitente) (logginLights 'en-rojo-intermitente 'en-verde)
 (logginLights 'en-verde 'en-verde-intermitente) (logginLights 'en-verde-intermitente 'en-amarillo)
(logginLights 'en-amarillo 'en-amarillo-intermitente) (logginLights 'en-amarillo-intermitente 'en-rojo)
 ))|#
(defun informe (color-actual cambio-color)
 (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output)
   (format stream "Informe de Ejecución del Sistema Semafórico~%")
   (format stream "=========================================~%")
   (logginLights color-actual cambio-color)
(format stream "~A: transicion: ~A --> ~A"
(local-time :format-timestring nil (local-time:now):format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2)))
(car (transicion color-actual cambio-color)) (caddr (transicion color-actual cambio-color)) )
(format stream "~% --- Fin del Informe ---") ) )


