<apply template="default">
  <ifLoggedOut>
    <div class="page-header"><h1>You are not logged in.</h1></div>
    Please <a href="/login">log in</a> or <a href="/register">register</a>.
  </ifLoggedOut>
  <ifLoggedIn>
    <div class="page-header"><h1>Hello, <username/></h1></div>
    <p>Your message is <message/>.</p>
    <form method="post" action="/">
      <input type="text" name="message" id="message"/>
	<button type="submit" class="btn">Set message</button>
    </form>
  </ifLoggedIn>
</apply>
