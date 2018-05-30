window.addEventListener("DOMContentLoaded", function(){
    var getStyle = function(el,styleProp){
        if(el.currentStyle){
            return el.currentStyle[styleProp];
        }else if(window.getComputedStyle){
            return document.defaultView.getComputedStyle(el,null).getPropertyValue(styleProp);
        }
        return null;
    };
    
    var getSourceLocation = function(){
        var hash = window.location.hash;
        if(hash){
            var colon = hash.indexOf(":");
            if(colon){
                return [ parseInt(hash.substr(1, colon)),
                         parseInt(hash.substr(colon+1)) ];
            } else {
                return [ parseInt(hash.substr(1)),
                         0 ];
            }
        }
        return null;
    };

    var scrollToLine = function(line, editor){
        editor = editor || document.querySelector(".file");
        var code = editor.querySelector("code.lisp");
        var lineHeight = Length.toPx(code, getStyle(code, "line-height"));
        var pos = code.getBoundingClientRect().top + (line-1) * lineHeight;
        console.log(pos);
        window.scrollTo(0, pos);
    };
    
    var highlightEditor = function(editor){
        var code = editor.querySelector("code.lisp");
        HighlightLisp.highlight_element(code);
    };

    var highlightAllEditors = function(root){
        root = root || document;
        [].forEach.call(root.querySelectorAll(".file"), highlightEditor);
    };

    highlightAllEditors();
    var location = getSourceLocation();
    if(location) scrollToLine(location[0]);
});
