module StoryManager(
    availableVerbs,
    availableNouns,
    availablePrepositions,
    availableKeywords,
    availableTokens,
    initialInventory,
    initialFlags,
    settings,
    scenes
) where

    import qualified Data.Map as Map

    import Lexer (Token(..), Command(..))
    import Parser (Sentence(..))
    import SpaceManager (Condition(..), State(..), ConditionalAction(..), Interaction(..),  Scene(..), Inventory(..), Flag(..), Settings(..))


    availableVerbs :: [Token]
    availableVerbs = [
        Verb "examinar" ["examinar", "abrir", "mirar", "observar", "analizar", "inspeccionar", "ver"],
        Verb "usar" ["usar", "abrir", "utilizar"]] 

    availableNouns :: [Token]
    availableNouns = [
        Noun "izquierda" ["izquierda"],
        Noun "derecha" ["derecha"],
        Noun "arriba" ["arriba"],
        Noun "abajo" ["abajo", "debajo"],
        Noun "delante" ["delante", "al frente", "adelante"],
        Noun "portavasos" ["portavasos"],
        Noun "asiento" ["asiento"],
        Noun "alfombra" ["alfombra"],
        Noun "llave" ["llave"],
        Noun "cinturon" ["cinturon"],
        Noun "destornillador" ["destornillador"],
        Noun "cuchillo" ["cuchillo"],
        Noun "luz" ["luz"],
        Noun "panel" ["panel", "tablero"],
        Noun "cables" ["cables"],
        Noun "guantera" ["guantera"],
        Noun "cinta" ["cinta"],
        Noun "caja" ["caja"],
        Noun "martillo" ["martillo"],
        Noun "ventana" ["ventana"],
        Noun "parabrisas" ["parabrisas"],
        Noun "interruptor" ["interruptor"],
        Noun "verde" ["verde"],
        Noun "rojo" ["rojo"],
        Noun "azul" ["azul"],
        Noun "blanco" ["blanco"],
        Noun "respiradero" ["respiradero"],
        Noun "trampilla" ["trampilla"],
        Noun "escritorio" ["escritorio"],
        Noun "carta" ["carta"],
        Noun "foto" ["foto"],
        Noun "cajon" ["cajon"],
        Noun "basura" ["basura"],
        Noun "palanca" ["palanca"],
        Noun "interruptor" ["interruptor"],
        Noun "puerta" ["puerta"],
        Noun "interruptor" ["interruptor"],
        Noun "0390" ["0390"],
        Noun "2415" ["2415"]] 

    availablePrepositions :: [Token]
    availablePrepositions = [ Preposition "en" ["en"], Preposition "con" ["con", "usando"] ] 

    availableKeywords :: [Token]
    availableKeywords = [
        Keyword "ayuda" ["ayuda"],
        Keyword "salir" ["salir"],
        Keyword "flags" ["flags"],
        Keyword "inventario" ["inventario"]]

    availableTokens :: [Token]
    availableTokens = availableVerbs ++ availableNouns ++ availablePrepositions ++ availableKeywords

    initialInventory :: Inventory
    initialInventory = Inventory []

    initialFlags :: Flag
    initialFlags = Flag (Map.fromList [("Auto", ["inicio", "cinturon"]), ("Oficina", ["inicio"])])

    settings :: Settings
    settings = Settings{
        gameName = "Scape the Room",
        defaultPhrase = "¿Que quieres hacer ahora?\n\n> ",
        defaultErrorPhrase = "No te he entendido. Esa no es una opción. Inspecciona un objeto, usa un artículo en un objeto o mira en una dirección diferente.\n",
        dynamic = True,
        help = "Comandos disponibles:\n\n"
                ++ "  - Inventario: Muestra todos los objetos en tu inventario.\n"
                ++ "  - Ayuda: Muestra este mensaje de ayuda.\n"
                ++ "  - Exit: Termina el juego.\n",
        quitMessage = "Gracias por jugar. ¡Hasta luego!\n",
        finishFlag = "completado"
    }

    scenes :: [Scene]
    scenes = [
        Scene {
            name = "Auto",
            canBeStartPoint = True,
            start = [
                ConditionalAction {
                    condition = ActiveFlag "inicio",
                    description = "Una luz brilla en tus ojos, despertándote de un profundo sueño. Todo lo que puedes recordar es que pasaste una noche en un bar bebiendo más de lo debido. Realmente necesito dejar de beber tanto, piensas. Cuando tus ojos se abren lentamente, te das cuenta de que esto no es una resaca común. Un cinturón de seguridad restringe tu movimiento hacia adelante. Intentas desabrocharlo, pero el pestillo está atascado. Cuando tiras del cinturón, este se niega a ceder, solo apretándose cada vez más. Miras a tu izquierda y luego a tu derecha, notando que las ventanas han sido reemplazadas por gruesas hojas de metal. En el frente puedes distinguir un parabrisas por donde pasa el resplandor. El pánico comienza cuando empiezas a preguntarte si alguna vez volverás a ver a tu familia. ¿Que te gustaría hacer? Solo di mirar hacia arriba, abajo, izquierda, derecha o adelante para comenzar.\n",
                    states = [
                        DeactivateFlag "inicio"
                    ]
                },
                ConditionalAction {
                    condition = No (ActiveFlag "inicio"),
                    description = "No te he entendido. Esa no es una opción. Inspecciona un objeto, usa un artículo en un objeto o mira en una dirección diferente.",
                    states = []
                }
            ],
            interactions = [
                Interaction {
                    command = Command ["examinar", "izquierda"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras a la izquierda. No hay mucho mas que el portavasos y una ventana.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["mirar", "portavasos"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "llave",
                            description = "El portavasos esta vacio.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "llave"),
                            description = "Miras el portavasos y encuentras una llave. La recoges.",
                            states = [
                                AddToInventory "llave"
                            ]
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "derecha"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras a la derecha pero no ves mucho. Las ventanas son de metal solido y el asiento a tu lado esta viejo y roto.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "asiento"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "destornillador",
                            description = "El asiento tiene un pequeño orificio de donde tomaste el destornillador.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "destornillador"),
                            description = "Miras el asiento y encuentras un destornillador. Lo recoges.",
                            states = [
                                AddToInventory "destornillador"
                            ]
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "arriba"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "cuchillo",
                            description = "Miras hacia arriba a la luz que abriste. No hay nada que hacer aqui.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "cuchillo"),
                            description = "Miras al techo, no hay nada mas que una luz. Parece que algo podria estar atascado alli, pero no puedes retirarlo",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "luz"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "cuchillo",
                            description = "Miras hacia arriba donde solia estar la cubierta de la luz. Solo hay una pequeña bombilla en su lugar.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "cuchillo"),
                            description = "Miras mas de cerca la luz. Intentas forzarla para que se abra pero esta atornillada fuertemente.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "destornillador", "en", "luz"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "destornillador",
                            description = "Usas el destornillador para abrir la cubierta de la luz y sale un cuchillo. Lo recoges.",
                            states = [
                                AddToInventory "cuchillo"
                            ]
                        },
                        ConditionalAction {
                            condition = No (InInventory "destornillador"),
                            description = "Lo sentimos pero no tienes un destornillador. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "abajo"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "cinturon",
                            description = "Miras hacia abajo y ves una alfombra que combina con tus zapatos. Casi como si lo hubieras cordinado.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (ActiveFlag "cinturon"),
                            description = "Miras hacia abajo y ves una alfombra que combina con tus zapatos. Casi como si lo hubieras cordinado. El cinturon de seguridad recortado se encuentra en la parte superior.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "alfombra"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Levantas la alformbra y rebelas una vieja placa oxidada. Esta dice MMCDXV.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "adelante"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "cinturon",
                            description = "Miras delante de ti pero no puedes ver mucho a traves del parabrisas tintado. Intentas echar un vistazo a los asientos delanteros pero el cinturon de seguridad esta demasiado ajustado.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "cables")),
                            description = "Miras en los asientos delanteros, ves la guantera y una caja fuerte en el asiento del pasajero. Parece que el panel de instrumentos ha sido manipulado. Te acercas al parabrisas para mirar hacia afuera, desearias estar libre.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (ActiveFlag "cables"),
                            description = "Miras en los asientos delanteros, ves la guantera y una caja fuerte en el asiento del pasajero. Un monton de cables estan saliendo donde solia estar el panel de instrumentos. Te acercas al parabrisas para mirar hacia afuera, desearias estar libre.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "cuchillo", "en", "cinturon"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "cinturon"),
                            description = "Ya el cinturon esta roto.",
                            states = []
                        },
                        ConditionalAction {
                            condition = InInventory "cuchillo",
                            description = "Usas el cuchillo para liberarte del cinturon de seguridad. Cae en el suelo y ahora eres libre de moverte por la cabina.",
                            states = [
                                DeactivateFlag "cinturon"
                            ]
                        },
                        ConditionalAction {
                            condition = No (InInventory "cuchillo"),
                            description = "Lo sentimos pero no tienes un cuchillo. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "panel"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "cables",
                            description = "El tablero de instrumentos fue removido y esta tirado en el suelo.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "cables")),
                            description = "Hay un tablero de instrumentos atornillado debajo de la columna de direccion.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "destornillador", "en", "panel"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "cables",
                            description = "El tablero de instrumentos fue removido y esta tirado en el suelo.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "cables")),
                            description = "Desatornillas el panel de instrumentos y revela un monton de cables.",
                            states = [
                                ActivateFlag "cables"
                            ]
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "cables"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "cables",
                            description = "Hay un monton de cables desconectados colgando.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (ActiveFlag "cables"),
                            description = "No ves ningun cable.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "llave", "en", "guantera"],
                    actions = [
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (InInventory "llave"),
                            description = "Desbloqueas la guantera con tu llave. En su interior encontraras una cinta aislante que recoges.",
                            states = [
                                ActivateFlag "guantera abierta",
                                AddToInventory "cinta aislante"
                            ]
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (InInventory "llave")),
                            description = "La guantera esta cerrada.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "destornillador", "en", "guantera"],
                    actions = [
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (InInventory "destornillador"),
                            description = "La guantera esta cerrada.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (InInventory "destornillador")),
                            description = "Lo sentimos pero no tienes un destornillador. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["abrir", "guantera"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "guantera abierta",
                            description = "La guantera esta vacia. No hay nada que hacer aqui.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "guantera abierta")),
                            description = "La guantera esta cerrada.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "cinta", "en", "cables"],
                    actions = [
                        ConditionalAction {
                            condition = And (ActiveFlag "cables") (InInventory "cinta aislante"),
                            description = "Comienzas a usar tu cinta electrica para conectar los cables de manera segura. La energia del auto regresa.",
                            states = [
                                ActivateFlag "electricidad",
                                WithdrawFromInventory "cinta aislante"
                            ]
                        },
                        ConditionalAction {
                            condition = No (ActiveFlag "cables"),
                            description = "No ves ningun cable.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "cinta aislante"),
                            description = "Lo sentimos pero no tienes una cinta aislante. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "caja"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "electricidad",
                            description = "Necesitas 4 digitos para abrir la caja fuerte. Pista: abrir caja con 1234",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "electricidad")),
                            description = "Presionas los botones de la caja fuerte pero no responde. Parece que no hay electricidad para abrirla.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "caja", "con", "2415"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "electricidad",
                            description = "La caja fuerte se abre. Respiras con alivio. Dentro de la caja fuerte encuentras un martillo brillante y lo tomas.",
                            states = [
                                ActivateFlag "caja abierta"
                            ]
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "electricidad")),
                            description = "Presionas los botones de la caja fuerte pero no responde. Parece que no hay electricidad para abrirla.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "martillo", "en", "ventana"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "martillo",
                            description = "La ventana es demasiado pequeña para salir, no tiene sentido romperla.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "martillo"),
                            description = "Lo sentimos pero no tienes un martillo. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "cuchillo", "en", "asiento"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "cuchillo",
                            description = "Si cortas el asiento no tendras un lugar comodo para sentarte antes de escapar.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "cuchillo"),
                            description = "Lo sentimos pero no tienes un cuchillo. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "destornillador", "en", "caja"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "caja abierta",
                            description = "La caja fuerte esta abierta y vacia. No necesitas usar ningun articulo en ella.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "caja abierta")),
                            description = "No hay tornillos visibles para abrir con un destornillador.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "llave", "en", "caja"],
                    actions = [
                        ConditionalAction {
                            condition = ActiveFlag "caja abierta",
                            description = "La caja fuerte esta abierta y vacia. No necesitas usar ningun articulo en ella.",
                            states = []
                        },
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cinturon")) (No (ActiveFlag "caja abierta")),
                            description = "No hay ninguna cerradura visible para abrir con una llave.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "martillo", "en", "parabrisas"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "martillo",
                            description = "Golpeas el parabrisas tan fuerte como puedes. Das un gran suspiro de alivio cuando sales cuidadosamente por el parabrisas y encuentras el camino de regreso a casa. Enhorabuena por haber sido capaz de escapar. No mucha gente ha llegado tan lejos.\n",
                            states = [
                                ActivateFlag "completado"
                            ]
                        },
                        ConditionalAction {
                            condition = No (InInventory "martillo"),
                            description = "Lo sentimos pero no tienes un martillo. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                }
            ]
        },
        Scene{
            name = "Oficina",
            canBeStartPoint = True,
            start = [
                ConditionalAction {
                    condition = ActiveFlag "inicio",
                    description = "Tus ojos se abren lentamente y ves de manera borrosa una habitación. Estás acostado de espaldas en un estado de confusión. A medida que tu visión comienza a aclararse, comienzas a notar objetos familiares. Revisas tus bolsillos y faltan todas tus posesiones. Saltas en estado de pánico y te lanzas a la puerta que tienes por delante. Lamentablemente, la puerta parece estar cerrada con llave. Estas atrapado en una oficina. ¿Que te gustaría hacer? Solo di mirar hacia arriba, abajo, izquierda, derecha o adelante para comenzar.\n",
                    states = [
                        DeactivateFlag "inicio"
                    ]
                },
                ConditionalAction {
                    condition = No (ActiveFlag "inicio"),
                    description = "No te he entendido. Esa no es una opción. Inspecciona un objeto, usa un artículo en un objeto o mira en una dirección diferente.",
                    states = []
                }
            ],
            interactions = [
                Interaction {
                    command = Command ["examinar", "derecha"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras a la derecha y ves una caja fuerte en el suelo y un interruptor en la pared.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "interruptor"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "electricidad"),
                            description = "Hay 4 interruptores de diferentes colores. Azul, rojo, blanco y verde. ¿En que orden te gustaria activarlos?",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "electricidad",
                            description = "La energia parece estar fluyendo sin problema.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["verde", "rojo", "azul", "blanco"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "electricidad"),
                            description = "La energia comienza a fluir a traves del interruptor. Se oye un ruido procedente de debajo de la alfombra.",
                            states = [
                                ActivateFlag "electricidad"
                            ]
                        },
                        ConditionalAction {
                            condition = ActiveFlag "electricidad",
                            description = "La energia parece estar fluyendo sin problema.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "izquierda"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras a la izquierda y ves un escritorio contra la pared y un cubo de basura blanco al lada. Parece que hay algunos articulos en el escritorio.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "arriba"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras al techo. Lo unico que ves arriba es un respiradero azul recien pintado",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "respiradero"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "respiradero abierto"),
                            description = "Miras hacia el respiradero arriba y ves que esta atornillado al techo.",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "respiradero abierto",
                            description = "Ves el respiradero que abriste, pero es demasiado pequeño para que entres.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "abajo"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras hacia abajo y ves una alfombra oriental polvorienta roja. Parece que ha visto dias mejores.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "alfombra"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Levantas la alfombra roja. Dentro de las polvorientas tablas en el suelo, observas lo que parece ser una trampilla.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "trampilla"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "electricidad"),
                            description = "Ves una trampilla debajo de la alfombra. Hay un interruptor al lado pero no parece estar funcionando.",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "electricidad",
                            description = "La trampilla esta abierta y tiene una palanca en su interior. La tomas.",
                            states = [
                                AddToInventory "palanca"
                            ]
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "delante"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras hacia delante de ti. Hay una puerta verde. Esta podria ser tu salida.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "escritorio"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Ves un hermoso escritorio de caoba con un cajon, una foto antigua y lo que parece ser una carta recientemente escrita.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "carta"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Empiezas a leer la carta que esta sobre el escritorio, 'Estimada secretaria: En el momento que cruzaste la puerta supe que eras la indicada para mi. Me sacaste de mis pies cuando nuestros ojos se cruzaron por primera vez. Odio tener que decirte todos mis sentimientos en una carta, pero esto tenia que ser dicho. Sin ti no puedo respirar. Espero que tengas el corazon de no tirar esta carta a la basura, como las ultimas 4 que te envie. Sinceramente tu jefe.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "foto"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "vidrio roto"),
                            description = "Una vieja foto se encuentra detras de un marco de vidrio grueso. Es un padre que celebra el quinto cumpleaños de su hija.",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "vidrio roto",
                            description = "El padre que celebra el quinto cumpleaños de su hija. Giras la foto y ves marcada la fecha como marzo de 1995.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "cajon"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "cajon abierto"),
                            description = "Intentas abrir el cajon del escritorio, pero parece estar atascado. Hay una pequeña abertura en la parte superior. Algo podria caber aqui.",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "cajon abierto",
                            description = "El cajon ya esta roto donde lo golpeaste. Ademas de eso esta vacio.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "basura"],
                    actions = [
                        ConditionalAction {
                            condition = Always,
                            description = "Miras en el cesto de basura blanco, pero no hay nada dentro.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "caja"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "caja abierta"),
                            description = "Esta caja fuerte tiene una combinacion de cuatro digitos. Pista: abrir caja con 1234",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "caja abierta",
                            description = "Ya has eliminado todo lo valioso de la caja fuerte.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["examinar", "caja", "con", "0390"],
                    actions = [
                        ConditionalAction {
                            condition = No (ActiveFlag "caja abierta"),
                            description = "La puerta se abre para pila llena de documentos legales, una pistola, algunas fotos y una llave. Recoges la llave para mas tarde, pero dejas el arma.",
                            states = [
                                ActivateFlag "caja abierta",
                                AddToInventory "llave"
                            ]
                        },
                        ConditionalAction {
                            condition = ActiveFlag "caja abierta",
                            description = "Ya la caja fuerta esta abierta.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "martillo", "en", "foto"],
                    actions = [
                        ConditionalAction {
                            condition = And (No (ActiveFlag "vidrio roto")) (InInventory "martillo"),
                            description = "Rompiste el vidrio con tu martillo y sacaste la foto. En la parte posterior hay una fecha. Marzo de 1995.",
                            states = [
                                ActivateFlag "vidrio roto"
                            ]
                        },
                        ConditionalAction {
                            condition = No (InInventory "martillo"),
                            description = "Lo sentimos pero no tienes un martillo. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "vidrio roto",
                            description = "El marco de la foto ya esta roto, no es necesario utilizar ningun articulo en el.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "destornillador", "en", "repiradero"],
                    actions = [
                        ConditionalAction {
                            condition = And (No (ActiveFlag "repiradero abierto")) (InInventory "destornillador"),
                            description = "Con el destornillador puedes retirar lentamente la ventilacion del techo. Mientras lo haces cae un martillo y lo recoges.",
                            states = [
                                AddToInventory "martillo"
                            ]
                        },
                        ConditionalAction {
                            condition = No (InInventory "destornillador"),
                            description = "Lo sentimos pero no tienes un destornillador. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "repiradero abierto",
                            description = "Ves el respiradero que abriste, pero es demasiado pequeño para que entres.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "palanca", "en", "cajon"],
                    actions = [
                        ConditionalAction {
                            condition = And (No (ActiveFlag "cajon abierto")) (InInventory "palanca"),
                            description = "Deslizas la palanca en la abertura y abres el cajon. Encuentras un destornillador dentro y lo recoges.",
                            states = [
                                AddToInventory "destornillador"
                            ]
                        },
                        ConditionalAction {
                            condition = No (InInventory "palanca"),
                            description = "Lo sentimos pero no tienes una palanca. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        },
                        ConditionalAction {
                            condition = ActiveFlag "cajon abierto",
                            description = "El cajon ya esta roto donde lo golpeaste. Ademas de eso esta vacio.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "martillo", "en", "caja"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "martillo",
                            description = "Intentas abrir la puerta de la caja fuerte con el martillo, pero es demasiado segura.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "martillo"),
                            description = "Lo sentimos pero no tienes un martillo. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "destornillador", "en", "caja"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "destornillador",
                            description = "No hay tornillos en la caja fuerte",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "destornillador"),
                            description = "Lo sentimos pero no tienes un destornillador. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "destornillador", "en", "puerta"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "destornillador",
                            description = "No hay tornillos en la puerta para retirarlos",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "destornillador"),
                            description = "Lo sentimos pero no tienes un destornillador. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "palanca", "en", "puerta"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "palanca",
                            description = "Intentas abrir la puerta con la palanca, pero no hay suficiente espacio para que quepa la palanca.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "palanca"),
                            description = "Lo sentimos pero no tienes una palanca. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "martillo", "en", "puerta"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "martillo",
                            description = "Llamas a la puerta con un martillo. ¿Hay alguien? Oh no, espera, fuiste tu martilleando.",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "martillo"),
                            description = "Lo sentimos pero no tienes un martillo. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                },
                Interaction {
                    command = Command ["usar", "llave", "en", "puerta"],
                    actions = [
                        ConditionalAction {
                            condition = InInventory "llave",
                            description = "Intentas meter la llave en la cerradura pero no encaja. Entonces le das la vuelta y entra perfectamente. Giras el picaporte y caminas hacia la libertad. Enhorabuena por haber sido capaz de escapar. No mucha gente ha llegado tan lejos.\n",
                            states = []
                        },
                        ConditionalAction {
                            condition = No (InInventory "llave"),
                            description = "Lo sentimos pero no tienes una llave. Mira el inventario para saber los articulos de los que dispones.",
                            states = []
                        }
                    ]
                }
            ]
        }]

