window.addEventListener("DOMContentLoaded", function(){
    var registerSearch = function(search, list){
        list = list || search.nextElementSibling;
        search.addEventListener("keyup", function(ev){
            [].forEach.call(list.children, function(child){
                if(child.textContent.indexOf(search.value) !== -1){
                    child.style.display = null;
                } else {
                    child.style.display = "none";
                }
            });
        });
    };

    var registerAllSearches = function(root){
        root = root || document;
        [].forEach.call(root.querySelectorAll(".search"), function(search){
            registerSearch(search);
        });
    };

    registerAllSearches();
});
