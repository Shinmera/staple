window.addEventListener("DOMContentLoaded", function(){
    var unmarkElement = function(el){
        if(el.tagName === "mark" || el.tagName === "MARK"){
            [].forEach.call(el.childNodes, function(child){
                el.parentNode.insertBefore(child, el);
            });
            el.parentNode.removeChild(el);
        }else if(el.parentNode.tagName === "mark"){
            return unmarkElement(el.parentNode);
        }
        return null;
    }

    var unmarkAll = function(root){
        root = root || document;
        [].forEach.call(root.querySelectorAll("mark"), unmarkElement);
    }
    
    var markElement = function(el){
        if(el.parentNode.tagName === "mark" || el.parentNode.tagName === "MARK"){
            return el.parentNode;
        } else {
            unmarkAll();
            var marked = document.createElement("mark");
            el.parentNode.insertBefore(marked, el);
            marked.appendChild(el);
            return marked;
        }
    }

    var markFragmented = function(){
        if(window.location.hash){
            var el = document.getElementById(decodeURIComponent(window.location.hash.substr(1)));
            if(el) markElement(el);
        }
    }

    var registerXrefLink = function(link){
        var el = document.getElementById(decodeURIComponent(link.getAttribute("href").substr(1)));
        if(el){
            link.addEventListener("click", function(){
                markElement(el);
            });
        }
    }

    var registerXrefLinks = function(root){
        root = root || document;
        [].forEach.call(root.querySelectorAll("a.xref"), registerXrefLink);
    }

    markFragmented();
    registerXrefLinks();
});
