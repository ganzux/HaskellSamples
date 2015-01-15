module Ejercicio_e where

{--
Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados en una 
universidad que pertenezcan a alguna de las asociaciones de ésta (culturales, deportivas, 
de representación estudiantil, etc.). Para ello se deberán crear nuevos tipos de datos que 
representen: 
• Estudiante, de cada uno se debe disponer del nombre y titulación 
• Titulación, que pueden ser tres: Grado II, Grado II_ADE, Grado ADE 
• Lista de estudiantes matriculados 
• Lista de estudiantes que pertenecen a asociaciones 
 Un ejemplo de aplicación de la función que se pide podría ser: 
> mostrarAlumnosAsociaciones(listaMatriculados,listaAsociaciones) 
 "(Carlos Calle,GradoADE_II)(Irene Plaza,GradoADE)"
Donde Carlos Calle e Irene Plaza son los únicos estudiantes matriculados que pertenecen a 
algún tipo de asociación en la universidad. 
-}

type Nombre = String 
data Titulacion = GradoIngInformatica | GradoADE_II | GradoADE deriving (Eq, Show) 
 
data Estudiante = Estu(Nombre, Titulacion) 
instance Eq Estudiante   where Estu(n1,t1) == Estu(n2,t2) = n1 == n2 && t1== t2 
instance Show Estudiante where show e = mostrarEstudiante e 
 
data ListaEstudiantesMatriculados = ListaMat [Estudiante] deriving Show 
data ListaEstudiantesAsociaciones = ListaAsoc [Estudiante] deriving Show 
e1:: Estudiante 
e1 = Estu("Pedro Rayo", GradoIngInformatica) 

e2:: Estudiante 
e2 = Estu("Carlos Calle", GradoADE_II) 

e3:: Estudiante 
e3 = Estu("Irene Plaza", GradoADE) 

e4:: Estudiante 
e4 = Estu("Carmen Palencia", GradoADE) 
listaMatriculados :: ListaEstudiantesMatriculados 
listaMatriculados = ListaMat[e1,e2,e3,e4] 
listaAsociaciones :: ListaEstudiantesAsociaciones 
listaAsociaciones = ListaAsoc[e2,e3] 
pertenece :: (Estudiante, ListaEstudiantesAsociaciones) -> Bool 
pertenece (e, ListaAsoc[]) = False 
pertenece (e, ListaAsoc(x:xs)) = e==x || pertenece(e,ListaAsoc(xs)) 
mostrarEstudiante :: Estudiante -> String 
mostrarEstudiante (Estu(nombre, titulacion)) = "(" ++ nombre ++ "," ++ show titulacion ++ ")"
mostrarAlumnosAsociaciones :: (ListaEstudiantesMatriculados,ListaEstudiantesAsociaciones) -> String 
mostrarAlumnosAsociaciones (ListaMat([]), listaAsoc) = ""
mostrarAlumnosAsociaciones (ListaMat(x:xs),listaAsoc) = if (pertenece(x,listaAsoc))
	then show x ++ mostrarAlumnosAsociaciones(ListaMat(xs),listaAsoc) 
	else mostrarAlumnosAsociaciones(ListaMat(xs),listaAsoc) 
