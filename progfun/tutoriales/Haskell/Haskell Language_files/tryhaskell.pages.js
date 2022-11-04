// Module for the guide pages
tryhaskell.pages = {};

// Unshow a string
tryhaskell.pages.unString = function (str) {
    return str.replace(/^"(.*)"$/, "$1").replace(/\\"/, '"');
}

// Random message from a list of messages
tryhaskell.pages.rmsg = function (choices) {
    return choices[Math.floor((Math.random() * 100) % choices.length)];
}

// Simple HTML encoding
tryhaskell.pages.htmlEncode = function (text, shy) {
    var x = $('<div></div>');
    x.text(text);
    return x.html();
}

// The nemesis
tryhaskell.nemesis = "chirs";

// All pages
tryhaskell.pages.list =
    [
        {
            title: '¿Hechamos un vistazo?',
            guide:
                '<div class="indent">' +
                '<h3>¿Hechamos un vistazo?</h3>' +
                '<p>Escribe <code>help</code>, <code>ayuda</code> o simplemente <code>?</code> para comenzar ' +
                'el tutorial.</p>' +
                '<p>O prueba escribiendo alguna de estas sentencias a ver qué pasa ' +
                '<p>' +
                '<code>23 * 36</code>, <code' +
                '>reverse ' +
                '"hello"</code> o <code>foldr (:) [] [1,2,3]</code>' +
                '</p>' +
                '<br><small class="note">Pro Tip: (puedes hacer click para insertar)</small>:</p>' +
                '</div>' +
                '</div>'
        },
        ////////////////////////////////////////////////////////////////////////
        // Lesson 1

        // Simple addition
        {
            lesson: 1,
            title: 'Basics; numbers, strings, etc.',
            guide:
                '<h3>' + tryhaskell.pages.rmsg(['Aprendiendo con números', 'Volviendo a lo básico', 'Tirando la matemática']).toString() + '</h3>'
                + "<p>Para comenzar, hagamos algunas cuentas. En la consola, puedes escribir"
                + " expresiones en Haskell. Prueba lo siguiente: <code>5 + 7</code></p>"
        },
        {
            guide: function (result) {
                if (!result) result = { expr: '5+7', value: 12 };
                var complied = result.expr.replace(/ /g, '') == "5+7";
                // var who = complied ? 'we' : 'you';
                var who = complied ? 'te sugerimos' : 'se te dió la gana';
                return '<h3>' + tryhaskell.pages.rmsg(['Tu primera expresión en Haskell',
                    "Aún recuerdo la primera vez"]) + '</h3>'
                    + '<p>Felicidades, lo hiciste genial! Se te retornó el número ' +
                    ' <code>' + result.value + '</code>. Usando la expresión ' + '<code>' +  result.expr + '</code>' +', justo como ' + who + '.'
                    + "</p><p>Intentemos ahora algo completamente distinto." +
                    " Escribe tu nombre. <br>Algo... Como esto:" +
                    ' <code>"chris"</code></p>' //! Change to your name
            },
            trigger: function (result) {
                return result.type.match(/^\(?Num [a-z]+\)? => [a-z]+$/) ||
                    result.type == "Integer" ||
                    result.type == "Int";
            }
        },
        // Strings & types
        {
            guide: function (result) {
                if (!result) result = { expr: '"chris"', value: "\"chris\"" };
                var n = tryhaskell.pages.unString(result.value); if (n) n = ", " + n;
                n += "!";
                return '<h3>' + tryhaskell.pages.rmsg(['Tipos de valores', "¿Qué hay en el nombre", "¿Y tu nombre es...?"]) +
                    '</h3>'
                    + '<p>Hola' + tryhaskell.pages.htmlEncode(n)
                    + (n != "!" ? ", mira que lindo nombre. Sinceramente." : "")
                    + " Creo que le estás cogiendo el tiro a esto! </p>" +
                    // "<p><strong>Note:</strong> You can chat to Haskell programmers while learning here, enter <code>chat</code> to start it."+
                    // " You will join the official IRC channel of the Haskell community!</p>"
                    "<p>Cada vez, estás obteniendo el valor de regreso de una expresión. Por el momento, " +
                    "sólo un número y una lista de caracteres (una cádena o string).</p>" +
                    "<p>También puedes obtener listas de otros tipos (no sólo caracteres). Miremos... ¿Que tal estos números para la lotería?: " +
                    " números de la suerte <code>["+ Array.from({length: Math.floor(Math.random() * 10) + 1}, () =>  Math.floor(Math.random() * 100)).toString() +"]</code></p>"
            },
            trigger: function (result) {
                return result.type == "[Char]"
                    || result.type == "String";
            }
        },
        // Overview of lesson 1
        {
            guide: function (result) {
                if (!result) result = { value: "[42,13,22]" };
                return '<h3>' + tryhaskell.pages.rmsg(["¡Terminaste tu primera lección!"]) +
                    '</h3>' +
                    "<p>¡Genial, ya has creado una lista de números! Si te ganas el baloto, repartimos " +
                    " las ganancias 😉</p>" +
                    "<p>Por ahora, veamos qué has aprendido:</p>" +
                    "<ol>" +
                    "<li>Cómo hacer operaciones aritméticas y listas de cosas.</li>" +
                    "</ol>" +
                    "<p>Puedes hacer varias cosas con las listas. Quizá quieras tus números del baloto ordenados " +
                    "de forma correcta. Prueba esto: " +
                    "<code>sort " + result.value + "</code></p>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*\[[0-9, ]+\][ ]*$/) &&
                    result.type.match(/^\(?Num [a-z]+\)? => \[[a-z]+\]$/);
            }
        },
        ////////////////////////////////////////////////////////////////////////
        // Lesson 2 - Functions
        // Functions on lists
        {
            lesson: 2,
            title: 'Simplemente funciones',
            guide: function (result) {
                if (!result) result = { value: "[13,23,30]" };
                return '<h3>' + tryhaskell.pages.rmsg(["Simplemente funciones", "¿Alguien dijo funciones?"]) +
                    '</h3>' +
                    "<p>Felicitaciones!, acabas de utilizar una <strong>función</strong>." +
                    " Así es como se hacen las cosas en Haskell." +
                    "<p>Como te habrás imaginado, obtuvimos de regreso <code>" +
                    tryhaskell.pages.htmlEncode(result.value)
                    + "</code>.</p><p>¿Alguna vez has querido saber cómo se llmaría tu gemelo malvado? También yo. " +
                    "Por suerte, puedes ordenar listas de caracteres o " +
                    "<strong>strings</strong>" +
                    ", de la misma forma que los números! <code>sort \"chris\"</code></p>"
            },
            trigger: function (result) {
                return result.expr.match(/sort/) &&
                    result.type.match(/\(?Num [a-z]+, Ord [a-z]+\)? => \[[a-z]+\]$/);
            }
        },
        // Tuples
        {
            guide: function (result) {
                if (!result) result = { value: "\"chirs\"" };
                tryhaskell.nemesis = tryhaskell.pages.htmlEncode(tryhaskell.pages.unString(result.value));
                return '<h3>' +
                    tryhaskell.pages.rmsg(["Tuplas... Porque a veces un sólo valor no es suficiente."]) +
                    '</h3>' +
                    "<p>Cuídate de " + tryhaskell.nemesis + "Deberías guardar su identificación para la policía.</p>" +
                    "<p>Mi gemelo malvado tiene 20 años: " +
                    "<code>(20,\"chirs\")</code></p>"
            },
            trigger: function (result) {
                return result.expr.match(/sort/) &&
                    result.type == "[Char]";
            }
        },
        // Functions on tuples
        {
            guide: function (result) {
                if (!result) result = { value: "(20,\"chirs\")" };
                var age = result.value.match(/^\(([0-9]+)+/);
                var villain = tryhaskell.pages.htmlEncode(result.value.replace(/\\"/g, '"'));
                return '<h3>' +
                    tryhaskell.pages.rmsg(["Lo guardaremos a salvo. No te preocupes."]) +
                    '</h3>' +
                    "<p>¿Ạ caso es normal que un villano tenga " +(age ? age[1] :"") + " años? " +
                    // "super-villain?</p>" +
                    "<p>¡Perfecto! Acabas de escribir una <em>tupla</em>. Es la forma de guardar un grupo de valores juntos en Haskell. " +
                    "Puedes colocar en ellas tantos valores como desees:</p>" +
                    "<ul><li><code>(1,\"sombreros\",23/35)</code></li><li><code>(\"Shaggy\",\"Daphnie\",\"Velma\")</code></li></ul>" +
                    "<p>De hecho, supongamos, que tu nemesis <em>es</em> " +
                    "<code>" + villain + "</code>" +
                    "¿Cómo averiguarías su edad?</p>" +
                    "<code>fst " + villain + "</code>"
            },
            trigger: function (result) {
                return result.type.match(/\(?Num [a-z]\)? => \([a-z], \[Char\]\)$/);
            }
        },
        // Summary of lesson 2
        {
            guide: function (result) {
                return '<h3>' +
                    tryhaskell.pages.rmsg(["¡Wow! Acabaste la lección 2",
                        "Lección 2 completa!"]) +
                    '</h3>' +

                    "<p>¡Buen trabajo! Acabas de obtener de regreso el valor de la edad de la tupla. " +
                    "Y sin una gota de sudor. ¿O sí?. La dunción <code>fst</code> " +
                    "regresa el <em>primer</em> valor de la tupla. Se llama \"fst\" porque " +
                    "es usado <em>BASTANTE</em> en Haskell así que debía ser realmente corta.</p>" +

                    "<p>Hora de tomar un break y ver qué hemos aprendido:</p>" +
                    "<ol>" +
                    "<li>Las funciones pueden ser usadas en listas de cualquier tipo.</li>" +
                    "<li>Podemos colocar valores ien tuplas.</li>" +
                    "<li>Obtener valores de tuplas es fácil.</li>" +
                    "</ol>" +

                    "<p>Ahora, supongamos que quieres usar un valor más deuna vez. " +
                    "¿Cómo lo harías? " +
                    "Para hacernos la vida más fácil, podemos hacer lo siguiente:</p>" +

                    "<code>let x = 4 in x * x</code>"
            },
            trigger: function (result) {
                return result.expr.match(/fst/) &&
                    result.type.match(/^\(?Num [a-z]\)? => [a-z]$/);
            }
        },
        {
            guide: function (result) {
                return "<h3>Fácil como andar en bicicleta</h3>" +

                    "<p>Acabas de <em>vincular (bound)</em> una <em>variable</em>. " +
                    "Así es, tu vinculaste <code>x</code> a la expresión <code>4</code>, " +
                    " y, ahora, puedes escribir <code>x</code> en un poco de código (el <em>cuerpo (body)</em>) y " +
                    " significará lo mismo como si escribieras <code>4</code>.</p>" +

                    "<p>Algo como esto: <code>let <em>var</em> = <em>expression</em> in <em>body</em></code></p>" +

                    "La palabra <code>in</code> sólo separa la expresión del cuerpo.</p>" +

                    "<p>Por ejemplo, intenta: " +
                    "<code><span class='highlight'>let</span> x <span class='highlight'>=</span> 8 * 10 <span class='highlight'>in</span> x + x</code></p>" +

                    "<p>Entonces, si sólo quisieramos obtener la edad de nuestro villano, podríamos hacer lo siguiente:</p>" +

                    "<code><span class='highlight'>let</span> villano <span class='highlight'>=</span> (28,\"chirs\") <span class='highlight'>in</span> fst villano</code>"

            }, trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]+x[ ]*=[ ]*[0-9]+[ ]*in[ ]*x[ ]*\*[ ]*x/) &&
                    result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
            }
        },
        {
            guide: function (result) {
                return "<h3>Acabamos lo básico. ¡SIUUU!</h3>" +
                    "<p>Ahora, veamos brevemente qué hemos aprendido " +
                    "<strong>Ázucar sintáctico</strong>. " +
                    "Intenta escribir esto:</p>" +
                    "<p><code>'a' : []</code></p>" +
                    "<p>O avanza a la <code>lección 4</code> para aprender acerca de funciones," +
                    " el grueso de Haskell!";
            }, trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]+villain[ ]*=[ ]*\([0-9]+,[ ]*"[^"]+"\)[ ]*in[ ]+fst[ ]+villain[ ]*/) &&
                    result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
            }
        },
        // Lesson 3: Syntactic sugar
        {
            lesson: 3,
            title: 'Ázucar síntactico',
            guide: function (result) {
                return '<h3>' +
                    tryhaskell.pages.rmsg(["Ya construíste una lista!"]) +
                    '</h3>' +
                    "<p>Well done, that was tricky syntax. You used the <code>(:)</code> " +
                    "function. It takes two values, some value and a list, and " +
                    " constructs a new list" +
                    " out of them. We call it 'cons' for short.</p>" +
                    "<p><code>'a'</code> is " +
                    "the character 'a', <code>[]</code> is an empty list. So " +
                    "tacking <code>'a'</code> at the start of an empty list just " +
                    "makes a list <code>['a']</code>!</p>" +
                    "<p>But thankfully we don't have to type out " +
                    "<code>'a' : 'b' : []</code> every time we want to make a " +
                    "list of characters; we can use " +
                    "<strong>syntactic sugar</strong> and just write" +
                    " <code>['a','b']</code>. Don't believe me, check this!</p>" +
                    "<code>'a' : 'b' : [] == ['a','b']</code>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*'a'[ ]*:[ ]*\[\][ ]*/) &&
                    result.type == "[Char]";
            }
        },
        // Booleans and string syntactic sugar
        {
            guide: function (result) {
                return '<h3>' +
                    tryhaskell.pages.rmsg(["You're on fire!"]) +
                    '</h3>' +
                    "<p>You're handling this syntax really well, nice!</p>" +
                    "<p>You just got a boolean value back, and it said " +
                    "<code>True</code>. That means they're equal!</p>" +
                    "<p>One final demonstration on syntactic sugar for now:</p>" +
                    "<code>['a','b','c'] == \"abc\"</code>"
            },
            trigger: function (result) {
                return result.type == "Bool" &&
                    result.expr.replace(/[^':\[\]\=,]/g, '') == "'':'':[]==['','']";
            }
        },
        // Summary of syntactic sugar section
        {
            guide: function (result) {
                return '<h3>' +
                    tryhaskell.pages.rmsg(["Lesson 3 over! Syntactic sugar is sweet"]) +
                    '</h3>' +
                    "<p>Let's have a gander at what you learned:</p>" +
                    "<ol>" +
                    "<li>In <code>'a' : []</code>, <code>:</code> is really just " +
                    " another function, just clever looking.</li>" +
                    "<li>Pretty functions like this are written like <code>(:)</code> when " +
                    " you talk about them.</li>" +
                    "<li>A list of characters <code>['a','b']</code> can just be written " +
                    "<code>\"ab\"</code>. Much easier!</li>"
                    + "</ol>" +
                    "<p>Phew! You're getting pretty deep! Your arch nemesis, " +
                    tryhaskell.nemesis + ", is gonna try to steal your " + tryhaskell.pages.rmsg(['mojo',
                        'pizza']) +
                    "! Let's learn a bit more about functions and passing " +
                    "them around. Try this:</p> <code>map (+1) [1..5]</code></p>";
            },
            trigger: function (result) {
                return result.expr.replace(/[^\]\[',=\"]?/g, '') == "['','','']==\"\"" &&
                    result.type == "Bool";
            }
        },
        {
            lesson: 4,
            title: 'Functions, reloaded; passing, defining, etc.',
            guide: function () {
                var title =
                    tryhaskell.pages.rmsg(["Functions [of a Geisha]",
                        "Functions, functors, functoids, funky",
                        "Functions: Expanded fo' real"]);
                return "<h3>" + title + "</h3>" +

                    "<p>Here's where the magic begins!</p>" +

                    "<p>You just passed the <code>(+1)</code> " +
                    "function to the <code>map</code> function.</p>" +

                    "<p>You can try other things like <small class='note'>(remember: click to insert them)</small>:</p>" +

                    "<ul>" +
                    "<li><code>map (*99) [1..10]</code></li>" +
                    "<li><code>map (/5) [13,24,52,42]</code></li>" +
                    "<li><code>filter (>5) [62,3,25,7,1,9]</code></li>" +
                    "</ul>" +

                    "<p>Note that a tuple is different to a list because you can do this:</p>" +
                    "<code>(1,\"George\")</code>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*map[ ]+\(\+1\)[ ]*\[1..5\][ ]*$/) &&
                    (result.type.match(/^\(?Num [a-z], Enum [a-z]\)? => \[[a-z]\]$/) ||
                        result.type.match(/^\(?Enum [a-z], Num [a-z]\)? => \[[a-z]\]$/));
            }
        },
        {
            guide: function (result) {
                return "<h3>Lists and Tuples</h3>" +

                    "<p>You can only " +
                    " have a list of numbers or a list of characters, whereas in a tuple you can throw anything in! </p>" +

                    "<p>We've also seen that you can make a new list with <code>(:)</code> that joins two values together, like: </p>" +
                    "<p><code>1 : [2,3]</code></p>" +

                    "<p>But we can't do this with tuples! You can only write a tuple and then look at what's inside. You can't make new ones on the fly like a list." +

                    "<p>Let's write our own functions! It's really easy. How about something simple:</p>" +
                    "<code>let square x = x * x in square " + tryhaskell.pages.rmsg([52, 10, 3]) + "</code>"

            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*\(1,"[^"]+"\)[ ]*$/) &&
                    result.type.match(/^\(?Num [a-z]\)? => \([a-z], \[Char\]\)$/);
            }
        },
        {
            guide: function (result) {
                return "<h3>Let there be functions</h3>" +
                    "<p>Nice one! I think you're getting used to the <code>let</code> syntax.</p>" +
                    "<p>You defined a function. You can read it as, as for a given " +
                    "<em>parameter</em> called <code>x</code>, <code>square</code> of " +
                    "<code>x</code> is <code>x * x</code>." +
                    "<p>Some others you can try are:</p>" +
                    "<ul><li><code>let add1 x = x + 1 in add1 5</code></li>" +
                    "<li><code>let second x = snd x in second (3,4)</code></li>" +
                    "</ul>" +
                    "<p>Let's go crazy and use our <code>square</code> function with map:</p>" +
                    "<code>let square x = x * x in map square [1..10]</code>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]*square[ ]+x[ ]*=[ ]*x[ ]*\*[ ]*x[ ]*in[ ]*square[ ]+[0-9]+/) &&
                    result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
            }
        },
        {
            guide: function (result) {
                if (!result || !result.value) result = { value: "[1,4,9,16,25,36,49,64,81,100]" };
                return "<h3>Let there be functions</h3>" +

                    "<p>That's so cool! You described a simple function <code>square</code> and then " +
                    "you just passed it to another function (<code>map</code>) and got back <code>" +
                    tryhaskell.pages.htmlEncode(result.value) + "</code>, exactly what you expected!</p>" +

                    "<p>Haskell is pretty good at composing things together like this. " +
                    "Some other things you can try are:</p>" +

                    "<ul>" +
                    "<li><code>let add1 x = x + 1 in map add1 [1,5,7]</code></li>" +
                    "<li><code>let take5s = filter (==5) in take5s [1,5,2,5,3,5]</code></li>" +
                    "<li><code>let take5s = filter (==5) in map take5s [[1,5],[5],[1,1]]</code></li>" +
                    "</ul>" +

                    "<p>Did you get back what you expected?</p>" +

                    "<p>One more example for text; how do you upcase a letter?</p>" +

                    "<p><code>toUpper 'a'</code></p>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]+square[ ]+x[ ]*=[ ]*x[ ]*\*[ ]*x[ ]*in[ ]+map[ ]+square[ ]*\[1..10\][ ]*$/) &&
                    (result.type.match(/^\(?Num [a-z], Enum [a-z]\)? => \[[a-z]\]$/) ||
                        result.type.match(/^\(?Enum [a-z], Num [a-z]\)? => \[[a-z]\]$/));
            }
        },
        {
            guide: function (result) {
                return "<h3>Exercise time!</h3>" +

                    "<p>Easy! Remember: characters are written like <code>'a'</code> and " +
                    "strings (lists of characters) are written like <code>\"a\"</code>." +

                    "<p>I need you to use <code>toUpper</code> capitalise my whole name, " +
                    "<code>\"Chris\"</code>. Give it a try." +
                    " You can do it, I believe in you!</p>" +

                    '<p>Spoiler: <code class="spoiler">map toUpper "Chris"</code></p>'
            },
            trigger: function (result) {
                return result.expr.match(/^toUpper 'a'$/) &&
                    result.type == "Char";
            }
        },
        {
            guide: function (result) {
                return "<h3>Lesson 4 complete!</h3>" +

                    "<p>Brilliant! You're making excellent progress! " +
                    "You just passed <code>toUpper</code> to <code>map</code>. No problem.</p>" +

                    "<p>Let's go over what you've learned in this lesson:</p>" +

                    "<ol>" +
                    "<li>Functions like <code>map</code> take other functions as parameters.</li>" +
                    "<li>Functions like <code>(+1)</code>, <code>(>5)</code> and " +
                    "<code>square</code> can be passed to other functions.</li>" +
                    "<li>Defining functions is just a case of writing what " +
                    "to do with the parameters.</li>" + "</ol>" +

                    "<p>Let's check out <em>pattern matching</em>; a way to " +
                    "get values from other values using patterns. Try this: </p>" +
                    "<p><code>let (a,b) = (10,12) in a * 2</code></p>"
            },
            trigger: function (result) {
                return result.type == "[Char]" &&
                    result.expr.match(/^map[ ]+toUpper/);
            }
        },
        {
            lesson: 5,
            title: 'Pattern Matching',
            guide: function (result) {
                var title =
                    tryhaskell.pages.rmsg(["And therefore, patterns emerge in nature.",
                        "And Then Patterns",
                        "Pattern matching!"])
                return "<h3>" + title + "</h3>" +

                    "<p>Jolly good show!</p>" +
                    "<p>So you had a value <code>(10,12)</code> and matched " +
                    "it against a pattern <code>(a,b)</code>, then you were able" +
                    " to do stuff with the <code>a</code> and <code>b</code>!" +

                    "<p>Note: Pattern matching <code>(a,b)</code> against " +
                    "<code>(1,2)</code> to get the <code>a</code> is the same as" +
                    " doing <code>fst (1,2)</code>, like you did in <code>step7</code>!</p>" +

                    "<p>A pattern always matches the way the " +
                    "value was originally constructed. Remember that <code>\"abc\"</code> is " +
                    "syntactic sugar for <code>'a' : 'b' : 'c' : []</code>.</p>" +

                    "<p>So you can get the characters from a string with patterns:</p>" +

                    "<code>let (a:b:c:[]) = \"xyz\" in a</code>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]+\(a,b\)[ ]+=[ ]+\(10,12\)[ ]+in[ ]+a[ ]*\*[ ]*2[ ]*$/) &&
                    result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
            }
        },
        {
            guide: function (result) {
                return "<h3>" + tryhaskell.pages.rmsg(["Ignorance is bliss", "Ignoring values"]) + "</h3>" +

                    "<p>You're getting into tricky syntax, huh? I know you can handle it!</p>" +

                    "<p>If you just want some of the values, you can ignore the others with <code>_</code> (underscore) like this:</p>" +

                    "<p><code>let (a:_:_:_) = \"xyz\" in a</code></p>" +

                    "<p>In fact, <code>(a:b:c:d)</code> is short-hand for " +
                    "<code>(a:(b:(c:d)))</code>, so you can just ignore the rest in one go:</p>" +

                    "<code>let (a:_) = \"xyz\" in a</code>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]+\(a:b:c:\[\]\)[ ]*=[ ]*\"xyz\"[ ]*in[ ]+a[ ]*$/) &&
                    result.type == "Char";
            }
        },
        {
            guide: function (result) {
                return "<h3>" + tryhaskell.pages.rmsg(["Exercise!", "Show me the money!"]) + "</h3>" +

                    "<p>Try to get the <code>'a'</code> value from this value using pattern matching:</p>" +
                    "<p><code>(10,\"abc\")</code></p>" +

                    "<p>Spoiler: <code class='spoiler'>let (_,(a:_)) = (10,\"abc\") in a</code></p>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]*\(a:_\)[ ]*=[ ]*"xyz"[ ]*in[ ]*a[ ]*$/) &&
                    result.type == "Char";
            }
        },
        {
            guide: function (result) {
                return "<h3>" + tryhaskell.pages.rmsg(["Well done!", "Brilliant!", "Perfetto!"]) + "</h3>" +

                    "<p>Wizard! I think you've got pattern-matching down.</p>" +

                    "<p>If you're still a bit unsure, here are some other things you can try:</p>" +

                    "<ul>" +
                    "<li><code>let _:_:c:_ = \"abcd\" in c</code></li>" +
                    "<li><code>let [a,b,c] = \"cat\" in (a,b,c)</code></li>" +
                    "</ul>" +

                    "<p>You can also grab a whole value <em>and</em> pattern match on it (have your cake and eat it too):</p>" +

                    "<code>let abc@(a,b,c) = (10,20,30) in (abc,a,b,c)</code>"
            },
            trigger: function (result) {
                return result.expr.match(/^[ ]*let[ ]*\(_,\(?a:_\)?\)[ ]*=[ ]*\(10,\"abc\"\)[ ]*in[ ]*a[ ]*$/) &&
                    result.type == "Char";
            }
        },
        {
            guide: function (result) {
                return "<h3>" + tryhaskell.pages.rmsg(["And that's the end of that chapter"]) + "</h3>" +

                    "<p>That was easy, right?</p>" +

                    "<p>Let's go over what you've learned in this lesson:</p>" +

                    "<ol>" +
                    "<li>Values are pattern matched, or <em>deconstructed</em>, by writing however they were constructed.</li>" +
                    "<li>Patterns let you use the values that you match.</li>" +
                    "<li>You can ignore whichever values you want.</li>" +
                    "<li>You can pattern match and keep hold of the original value too.</li>" +
                    "</ol>" +

                    "<p>Okay! That's all for now. It's time to dig into some <a href='/documentation'>documentation</a>!</p>"

            },
            trigger: function (result) {
                return result.type.match(/Num/)
            }
        }
    ];
