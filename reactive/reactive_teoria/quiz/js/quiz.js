(function(){
    function $(sel, ctx){ return (ctx||document).querySelector(sel); }
    function create(tag, cls){ var e=document.createElement(tag); if(cls) e.className=cls; return e; }

    function renderQuestion(item, idx){
        var wrap = create('div','question');
        var q = create('h3'); q.textContent = item.q; wrap.appendChild(q);
        var ul = create('ul','choices');
        item.choices.forEach(function(ch, i){
            var li = create('li');
            var id = 'q'+idx+'-c'+i;
            var input = document.createElement('input'); input.type='radio'; input.name='q'+idx; input.id=id; input.value=i;
            var label = create('label'); label.setAttribute('for', id); label.textContent = ch;
            li.appendChild(input); li.appendChild(label); ul.appendChild(li);
        });
        wrap.appendChild(ul);
        return wrap;
    }

    function buildQuiz(questions){
        var container = $('#quiz-container'); container.innerHTML='';
        questions.forEach(function(q,i){ container.appendChild(renderQuestion(q,i)); });
    }

    function grade(questions){
        var score=0, total=questions.length;
        questions.forEach(function(q,i){
            var sel = document.querySelector('input[name="q'+i+'"]:checked');
            var user = sel ? Number(sel.value) : null;
            var row = document.querySelectorAll('#quiz-container .question')[i];
            row.classList.remove('correct','wrong');
            if(user === q.answer){ score++; row.classList.add('correct'); }
            else { row.classList.add('wrong'); }
        });
        return {score: score, total: total};
    }

    function showResult(res){
        var r = $('#result'); r.innerHTML = '<strong>Resultado:</strong> '+res.score+' / '+res.total;
    }

    document.addEventListener('DOMContentLoaded', function(){
        if(!window.questions || !questions.length){ document.getElementById('quiz-container').textContent = 'No hay preguntas.'; return; }
        buildQuiz(questions);
        $('#submit').addEventListener('click', function(){ var res = grade(questions); showResult(res); });
        $('#restart').addEventListener('click', function(){ buildQuiz(questions); $('#result').textContent = ''; });
    });

})();
