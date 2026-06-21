(load "[ruta del quicklisp]quicklisp/setup.lisp")  ; tiene que ingresar la direccion donde se encuentra quicklisp
(ql:quickload "local-time")
;;: -------------------------------------------------------------------------------------------------------------------
;;; Funcion: Transicion
;;; NATURALEZA: Pura
;;; ESTRATEGIA: estructura condicional (implementada con COND), no recursiva, no predicado, no utiliza funciones de orden superior
;;; IMPACTO: No destructiva
;;; -------------------------------------------------------------------------------------------------------------------
(defun transicion (color-actual cambiar-a)
    (cond
        ((and (eql color-actual 'en-verde-intermitente) (eql cambiar-a 'cambiar-a-amarillo)) (list color-actual "CAMBIAR-A-AMARILLO" ))
        ((and (eql color-actual 'en-amarillo-intermitente) (eql cambiar-a 'cambiar-a-rojo)) (list color-actual "CAMBIAR-A-ROJO"))
        ((and (eql color-actual 'en-rojo-intermitente) (eql cambiar-a 'cambiar-a-verde)) (list color-actual "CAMBIAR-A-VERDE" ))
		((and (eql color-actual 'en-verde) (eql cambiar-a 'cambiar-a-verde-intermitente)) (list color-actual "CAMBIAR-A-VERDE-INTERMITENTE"))
		((and (eql color-actual 'en-amarillo) (eql cambiar-a 'cambiar-a-amarillo-intermitente)) (list color-actual "CAMBIAR-A-AMARILLO-INTERMITENTE"))
		((and (eql color-actual 'en-rojo) (eql cambiar-a 'cambiar-a-rojo-intermitente)) (list color-actual "CAMBIAR-A-ROJO-INTERMITENTE"))
        (t (list color-actual 'accion-por-defecto))
    )
)

;; Casos de prueba
;; (EN-VERDE-INTERMITENTE "CAMBIAR-A-AMARILLO")
(transicion 'en-verde-intermitente 'cambiar-a-amarillo)

;; (EN-VERDE "CAMBIAR-A-VERDE-INTERMITENTE")
(transicion 'en-verde 'cambiar-a-verde-intermitente)

;; (EN-ROJO "CAMBIAR-A-ROJO-INTERMITENTE")
(transicion 'en-rojo 'cambiar-a-rojo-intermitente)

;; (EN-ROJO-INTERMITENTE "CAMBIAR-A-VERDE")
(transicion 'en-rojo-intermitente 'cambiar-a-verde)

;; (EN-AMARILLO-INTERMITENTE "CAMBIAR-A-ROJO")
(transicion 'en-amarillo-intermitente 'cambiar-a-rojo)

;; (EN-VERDE ACCION-POR-DEFECTO)
(transicion 'en-verde 'cambiar-a-amarillo-intermitente)

;; (EN-AMARILLO "CAMBIAR-A-AMARILLO-INTERMITENTE")
(transicion 'en-amarillo 'cambiar-a-amarillo-intermitente)


;;; -------------------------------------------------------------------------------------------------------------------
;;; Funcion: timer
;;; Naturaleza: Pura
;;; Estrategia: estructura condicional(implementada con COND y MOD), 
;;; no recursiva, no predicado, no utiliza funciones de orden superior
;;; Impacto En Memoria: No Destructiva, no realiza cambios
;;; -------------------------------------------------------------------------------------------------------------------
(defun timer (timestap)
        (cond			; (89 92 212 215 221 224)
            ((<= 0 (mod timestap 225) 89) 'en-rojo )
            ((<= 90 (mod timestap 225) 92) 'en-rojo-intermitente )
            ((<= 93 (mod timestap 225) 212) 'en-verde )
            ((<= 213 (mod timestap 225) 215) 'en-verde-intermitente )
            ((<= 216 (mod timestap 225) 221) 'en-amarillo )
            (t 'en-amarillo-intermitente) 
    )
)

;; Casos de prueba
;; EN-VERDE-INTERMITENTE
(timer 214)

;; EN-AMARILLO
(timer 3592)

;; EN-AMARILLO
(timer 3593)

;; EN-AMARILLO
(timer 3595)

;; EN-AMARILLO-INTERMITENTE
(timer 3598)

;; EN-ROJO-INTERMITENTE
(timer 5716)

;; EN-ROJO
(timer 5713)

;; EN-VERDE
(timer 5723)


;;; -------------------------------------------------------------------------------------------------------------------
;;; Funcion: Loggin-Lights
;;; Naturaleza: Impura (por FORMAT T escribe en pantalla segun los datos de entrada)
;;; Estrategia: Simple (implementada con FORMAT), no recursiva, 
;;; no predicado, no utiliza funciones de orden superior
;;; Impacto En Memoria: No Destructiva, no realiza cambios
;;; -------------------------------------------------------------------------------------------------------------------
;; correcion: se automatizo el tiempo para que sea calculado directamente dentro de esta funcion,
;; en vez de un tiempo que puede estar
;; desactualizado, restamos al tiempo el cual esta dado desde 1970 unos 70 años
;; para que se sicronicen correctamente. 

(defun loggin-Lights (color-actual cambio-color unix-temp)
    (format t "~% Tiempo ~A la luz ah cambiado de color ~A a ~A~%" 
		(local-time:format-timestring nil (local-time:unix-to-timestamp unix-temp):format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
			(first (transicion color-actual cambio-color)) (second (transicion color-actual cambio-color))
	)
) 

;; Casos de prueba
;; Tiempo 2026-06-15 17:47:33 la luz ah cambiado de color EN-VERDE a CAMBIAR-A-AMARILLO
;; NIL
(loggin-Lights 'en-verde 'cambiar-a-amarillo 1781556453)


;; Tiempo 2026-06-15 17:47:33 la luz ah cambiado de color EN-VERDE a NIL 
;; NIL
(loggin-Lights 'en-verde 'cambiar-a-rojo 1781556453);caso de error


;; Tiempo 2026-06-15 17:48:12 la luz ah cambiado de color EN-ROJO a CAMBIAR-A-VERDE
;; NIL
(loggin-Lights 'en-rojo 'cambiar-a-verde 1781556492)


;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCIÓN: duracion-ciclo
;;; NATURALEZA: Pura
;;; ESTRATEGIA: secuancial (no recursiva, no predicado, no utiliza funciones de orden superior)
;;; IMPACTO: No destructiva
;;; -------------------------------------------------------------------------------------------------------------------

;; requerimiento 4.a 
(defun duracion-ciclo (rojo verde amarillo)
    (+ rojo verde amarillo 9) ; 9 de intermitencia
)

;; CASOS DE PRUEBA
;; 225
(duracion-ciclo 90 120 6)


;; otro ciclo
;; 114
(duracion-ciclo 40 60 5)


;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCIÓN: recomendacion-ciclo
;;; NATURALEZA: Pura
;;; ESTRATEGIA: condicional simple (condicional IF), no recursiva,
;;; no predicado, no utiliza funciones de orden superior
;;; IMPACTO: No destructiva
;;; -------------------------------------------------------------------------------------------------------------------

;; requerimiento 4.b
(defun recomendacion-ciclo (duracion)
    (if (and (>= duracion 35) (<= duracion 150)) 
		"ciclo optimo" 
		"ciclo no optimo"
	)
)
	
;; Casos de prueba
;; "ciclo no optimo"
(recomendacion-ciclo (duracion-ciclo 90 120 6 ))

;; "ciclo optimo"
(recomendacion-ciclo (duracion-ciclo 40 60 6 ))

;; "ciclo no optimo"
(recomendacion-ciclo (duracion-ciclo 7 12 5 ))


;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCIÓN: ciclos-por-tiempo
;;; NATURALEZA: Impura (escribe en pantalla un resultado dependiendo de los minutos de entrada)
;;; ESTRATEGIA: estructura secuencial (no presenta recursion en su implementacion), 
;;; no recursiva, no predicado, no utiliza funciones de orden superior
;;; IMPACTO: No destructiva
;;; -------------------------------------------------------------------------------------------------------------------
(defun ciclos-por-tiempo (minutos)
    (print "La cantidad de ciclos es de:")
    (print (truncate (/ (* minutos 60) 225)))
	; truncate toma el resultado de una operacion y elimina el decimal, si el resultado es 28.9, quedaria 28
)
;; Casos de prueba
;; "La cantidad de ciclos es de: 4"
(ciclos-por-tiempo 15)

;; "la cantidad de ciclos es de: 18"
(ciclos-por-tiempo 70)


;;; -------------------------------------------------------------------------------------------------------------------   
;;; FUNCION AUXILIAR: Calcular-Resto-Ini
;;; NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
;;; ESTRATEGIA: alternativa Multiple (cond), no recursiva, 
;;; no predicado, no utiliza funciones de orden superior
;;; IMPACTO: No destrutiva
;;; -------------------------------------------------------------------------------------------------------------------
(defun calcular-resto-ini (resto-ini)
	(cond 		; (89 92 212 215 221 224)
        ((<= 0 resto-ini 89) (list (- 90 resto-ini) 3 120 3 6 3)) ; (- 90 resto-ini) --> indica lo consumido por rojo
        ((<= 90 resto-ini 92) (list 0 (- 93 resto-ini) 120 3 6 3)) ; (- 93 resto-ini) --> indica lo consumido por el rojo-intermitente
		((<= 93 resto-ini 212) (list 0 0 (- 213 resto-ini) 3 6 3)) ; (- 213 resto-ini) --> indica lo consumido por verde
        ((<= 213 resto-ini 215) (list 0 0 0 (- 216 resto-ini) 6 3)) ; (- 216 resto-ini) --> indica lo consumido por verde-intermitente
		((<= 216 resto-ini 221) (list 0 0 0 0 (- 222 resto-ini) 3)) ; (- 222 resto-ini) --> indica lo consumido por amarillo
        (t (list 0 0 0 0 0 (- 225 resto-ini))) ; (-225 resto-ini) --> indica lo consumido por amarillo-intermitente
    )
)

;; CASOS DE PRUEBA
;; (0 0 0 0 0 -5330)
(calcular-resto-ini 5555) ; caso imposible y erroneo

;; (0 0 3 3 6 3)
(calcular-resto-ini 210)

;; (89 3 120 3 6 3)
(calcular-resto-ini 1)

;; (90 3 120 3 6 3)
(calcular-resto-ini 0) ; caso imposible pero correcto

;; (81 3 120 3 6 3)
(calcular-resto-ini 9)

	  
;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCION AUXILIAR: Calcular-Resto-Fin
;;; NATURALEZA: pura (dependiendo del resto que recibe, retorna un resultado)
;;; ESTRATEGIA: alternativa Multiple (cond), no recursiva, no predicado, no utiliza funciones de orden superio
;;; IMPACTO: No destrutiva
;;; -------------------------------------------------------------------------------------------------------------------

(defun calcular-resto-fin (resto-fin)
	(cond  	; (89 92 212 215 221 224)
        ; Al final de la hora es al revés: calculamos cuánto se consumió desde el inicio de ese último ciclo incompleto
        ; hasta llegar al segundo 'resto-fin'
        ((<= 0 resto-fin 89) (list resto-fin 0 0 0 0 0)) 
        ; resto-fin -->  indica lo consumido por rojo
        ((<= 90 resto-fin 92) (list 90 (- resto-fin  90) 0 0 0 0)) 
        ; (- resto-fin 90) --> indica lo consumido por el rojo-intermitente
		((<= 93 resto-fin 212) (list 90 3 (- resto-fin  93) 0 0 0)) 
        ; (- resto-fin  93) --> indica lo consumido por verde
        ((<= 213 resto-fin 215) (list 90 3 120 (- resto-fin  213) 0 0)) 
        ; (- resto-fin  213) --> indica lo consumido por verde-intermitente
		((<= 216 resto-fin 221) (list 90 3 120 3 (- resto-fin  216) 0)) 
        ; (- resto-fin  221) --> indica lo consumido por amarillo
        (t (list 90 3 120 3 6 (- resto-fin  222))) 
        ; (- resto-fin  222) --> indica lo consumido por amarillo-intermitente
    )
)

;; CASOS DE PRUEBA
;;(90 3 120 3 6 5333)
(calcular-resto-fin 5555) ; mismo caso imposible

;; (90 3 110 0 0 0)
(calcular-resto-fin 203)

;; (89 0 0 0 0 0)
(calcular-resto-fin 89)

;; (87 0 0 0 0 0)
(calcular-resto-fin 87)

;; (90 3 27 0 0 0)
(calcular-resto-fin 120)

;; (0 0 0 0 0 0)
(calcular-resto-fin 0) ; caso imposible pero correcto


;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCION AUXILIAR: calcular-Porcentajes
;;; NATURALEZA: Pura (devuelve una lista con los porcentajes de cada estado del semáforo)
;;; ESTRATEGIA: Secuencial (implementamos mediante operaciones aritméticas y la construcción de listas),
;;; no recursiva, no predicado, no utiliza funciones de orden superio
;;; IMPACTO: No destructiva
;;; -------------------------------------------------------------------------------------------------------------------

;;como ahora el ciclo es de 225, en 3600 entran 16 ciclos exactos, lo que se sabe es que 15 entran si o si,
;;y hay que determinar que paso con el ultimo,
;; si es que esta completo, o solo entro una parte

(defun calcular-Porcentajes (lista-ini lista-fin)
	(list ; todo basado en la regla de 3 simples
        (float (/ (* (+ 1350 (first lista-fin) (first lista-ini)) 100) 3600)) ; rojo
	    (float (/ (* (+ 45 (second lista-fin) (second lista-ini)) 100) 3600)) ; rojo-intermitente
	    (float (/ (* (+ 1800 (third lista-fin) (third lista-ini)) 100)3600)) ; verde
        (float (/ (* (+ 45 (fourth lista-fin) (fourth lista-ini)) 100) 3600)) ; verde-intermitente
        (float (/ (* (+ 90 (fifth lista-fin) (fifth lista-ini)) 100) 3600)) ; amarillo
        (float (/ (* (+ 45 (sixth lista-fin) (sixth lista-ini)) 100) 3600)) ; amarillo-intermitente
    )
)

;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCION: distribucion-Time
;;; NATURALEZA: impura (escribe en pantalla y en un archivo)
;;; ESTRATEGIA: secuencial, condicional doble (implementada con if y format),
;;; no recursiva, no predicado, no utiliza funciones de orden superio
;;; IMPACTO: no destructiva
;;; -------------------------------------------------------------------------------------------------------------------
(defun distribucion-Time (unix)
	(if (zerop (mod unix 225)) "| 40.0% rojo| 1.33% rojo-intermitente | 53.33% verde| 1.33% verde-intermitente| 2.66% amarillo| 1.33% amarillo intermitente|"
	    (format nil "~%| ~A% rojo| ~A% rojo-intermitente| ~A% verde | ~A% verde-intermitente| ~A% amarillo| ~A% amarillo-intermitente|"
	      (first (calcular-Porcentajes (calcular-Resto-Ini (mod unix 225)) (Calcular-Resto-Fin (mod (- 3600 (- 225 (mod unix 225))) 225)))) 
		  (second (calcular-Porcentajes (calcular-Resto-Ini (mod unix 225)) (Calcular-Resto-Fin (mod (- 3600 (- 225 (mod unix 225))) 225))))
	      (third (calcular-Porcentajes (calcular-Resto-Ini (mod unix 225)) (Calcular-Resto-Fin (mod (- 3600 (- 225 (mod unix 225))) 225))))
          (fourth (calcular-Porcentajes (calcular-Resto-Ini (mod unix 225)) (Calcular-Resto-Fin (mod (- 3600 (- 225 (mod unix 225))) 225))))
          (fifth (calcular-Porcentajes (calcular-Resto-Ini (mod unix 225)) (Calcular-Resto-Fin (mod (- 3600 (- 225 (mod unix 225))) 225))))
          (sixth (calcular-Porcentajes (calcular-Resto-Ini (mod unix 225)) (Calcular-Resto-Fin (mod (- 3600 (- 225 (mod unix 225))) 225))))))					
)

;; CASOS DE PRUEBA
;; lo cambiamos a first/second ya que utilizar car/cdr duplicaba o utilizaba algunos valores erroneos
;; y utilizar nth no cumplia con el esstilo
;; una cosa importante, es que al estar actualizado con ciclo de 225,
;; provoca que entren en 1 hora exactamente 16 cilos enteros
;; provocando que todo ciclo que no haya sido consumido al principio,
;; sea consumido al final, dando casi siempre los mismo ciclos
;; ciclos sin restos 0

;; "| 40.0% rojo| 1.3333334% rojo-intermitente| 53.333332% verde| 1.3333334% verde-intermitente| 2.6666667% amarillo| 1.3333334% amarillo-intermitente|"
(distribucion-Time 1782065340)

;; "| 40.0% rojo| 1.3333334% rojo-intermitente| 53.333332% verde| 1.3333334% verde-intermitente| 2.6666667% amarillo| 1.3333334% amarillo-intermitente|"
(distribucion-Time 6875458)

;; "| 40.0% rojo| 1.33% rojo-intermitente | 53.33% verde| 1.33% verde-intermitente| 2.66% amarillo| 1.33% amarillo intermitente|"
(distribucion-Time 3600) ;ciclo con resto 0

;; "| 40.0% rojo| 1.3333334% rojo-intermitente| 53.333332% verde | 1.3333334% verde-intermitente| 2.6666667% amarillo| 1.3333334% amarillo-intermitente|"
(distribucion-Time 5549)

;;; *********************************
;;; Extension 2, sistema de datos
;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCION: crear-informe
;;; NATURALEZA: inpura (escribe en el archivo)
;;; ESTRATEGIA: Utiliza una estructura condicional
;;; IMPACTO: no destructiva
;;; -------------------------------------------------------------------------------------------------------------------
(defun crear-informe ()
  (unless (probe-file "informe-ejecucion-semaforo.txt") ;probe-file comprueba si existe el archivo.txt, 
	  ;si existe, da verdadero, sino falso, y un unless es como if, pero solo ejecuta si la condicion es falsa
    (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output)
      (format stream "Informe de Ejecución del Sistema Semaforico~%")
      )
    )
  )

;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCION: cerrar-informe
;;; NATURALEZA: inpura (escribe en el archivo)
;;; ESTRATEGIA: finaliza el archivo
;;; IMPACTO: no destructiva
;;; -------------------------------------------------------------------------------------------------------------------
(defun cerrar-informe()
   (with-open-file (stream "informe-ejecucion-semaforo.txt":direction :output :if-exists :append)
       (format stream "~% --- Fin del Informe ---~%")
   )
 )

;;; -------------------------------------------------------------------------------------------------------------------
;;; FUNCION: informe
;;; NATURALEZA: impura (escribe en pantalla y en un archivo)
;;; ESTRATEGIA: Secuencial
;;; IMPACTO: no destructiva
;;; -------------------------------------------------------------------------------------------------------------------

(defun informe (color-actual cambio-color unix-temp)
    (crear-informe)
 	(with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :append :if-does-not-exist :create)
   	(format stream "~%=========================================~%")
   	(format stream "~A transicion: ~A --> ~A~%"
   	(local-time:format-timestring nil (local-time:unix-to-timestamp unixtemp)
	:format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
    (car (transicion color-actual cambio-color)) (caddr (transicion color-actual cambio-color)) )
 	)
	; pasandole loggin para ver en pantalla
   (logginLights color-actual cambio-color unix-temp)
)

(informe 'en-verde 'cambiar-a-amarillo 1742163000)
(informe 'en-verde 'cambiar-a-rojo 1742163000)
(informe 'en-amarillo 'cambiar-a-rojo 1718484493)
                                   
;; cierra el informe
(cerrar-informe)
