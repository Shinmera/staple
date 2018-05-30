window.addEventListener("DOMContentLoaded", function(){
    var filterChildren = function(container, filter){
        [].forEach.call(container.children, function(child){
            if(child.textContent.indexOf(filter) !== -1){
                child.style.display = null;
            } else {
                child.style.display = "none";
            }
        });
    };
    
    var registerSearch = function(search, list){
        list = list || search.nextElementSibling;
        search.addEventListener("keyup", function(ev){
            filterChildren(list, search.value);
        });
        filterChildren(list, search.value);
    };

    var registerAllSearches = function(root){
        root = root || document;
        [].forEach.call(root.querySelectorAll(".search"), registerSearch);
    };

    registerAllSearches();
});
