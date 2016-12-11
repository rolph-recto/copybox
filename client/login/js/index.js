$("#login").submit(function (event) {
    var tryLoginUrl = $("#login").attr("action");
    $("#login").attr("action", tryLoginUrl + location.search);
    return;
});
