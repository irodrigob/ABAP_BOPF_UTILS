# Objetivo/Objective
Clase que contiene utilidades para el tratamiento de BOPF / Class containing utilities for the treatment of BOPF

Dentro del proyecto hay una serie de programas de ejemplo donde se ve el funcionamiento de las parte principales de la clase. / Within the project there are a series of example programs where the operation of the main parts of the class is seen.

# Contenido / Contenido


- Class ZCL_CA_BOPF_UTIL
  - Método / Method CONSTRUCTOR -> Se le pasa el ID del BOPF que se va tratar / You pass the BOPF ID to be treated
  - Metodos / Methods SAVE_DATA, MODIFY_DATA y MODIFY_SAVE_DATA -> Métodos que encapsulan las llamadas de modificación y grabación de datos en el BOPF / Methods that encapsulate data modification and recording calls in the BOPF
  - Método / Method CONV_MESSAGE_BOPF_2_RETURN -> Convierte los mensaje de tipo BOPF (deben ser del tipo SYMSG) a tabla de mensajes de BAPI(BAPIRET2_T) / Convert messages of type BOPF (must be of type SYMSG) to BAPI message table (BAPIRET2_T).

# Dependencias/Dependencies

Ninguna / any

# Prerequisitos / Prerequisites

Versión ABAP 7.4 o superior / ABAP 7.4 or higher
