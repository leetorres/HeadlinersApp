function login(){
    $(document).on('click', '#login', function(){
	var $email = $('#email').val();
	var $password = $('#password').val();

	$.ajax({
	    url: "/search/login/" + $email + "/" + $password,
	    dataType: "json",

	}).done(function(data){

	    if(data.result == "success"){

		$('#loginPanel').remove();
		var accountPanel = "<div id='accountPanel'> <ul class='nav'>	<li> <a href='/favorites'>" +
		    "Favorites </a> </li> <li > <a href='/logout' id='logout'> Logout </a> </li> </ul> </div>"
		$('#menuBar').append(accountPanel);
	    }
	    else
		alert('email: ' + $email + ',password: ' + $password);

	});// end done

	return false;
    });// end click
}

function logout(){

    $(document).on('click', '#logout', function(){

	$.ajax({
	    url: "/search/logout/",
	    dataType: "json",
	    
	}).done(function(data){

	    $('#accountPanel').remove();
	    
	    if(data.result == "success"){
	    var loginPanel = "<div id='loginPanel'><form class='navbar-form pull-right'>" +
		"<input name='email'id='email' type='email' placeholder='Email' class='span2'>" +
		"<input name='password' id='password' type='password' placeholder='Password' class='span2'>" +
		"<button id ='login' class='btn'> Sign in </button>"  +
		"</form></div>";
	    $('#menuBar').append(loginPanel);
	    }
	});
	return false
    }); //end click

}// end logout
