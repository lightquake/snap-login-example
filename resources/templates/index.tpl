<apply template="default">
  <ifLoggedOut>
    <div class="page-header"><h1>You are not logged in.</h1></div>
    Please <a href="/login">log in</a> or <a href="/register">register</a>.
  </ifLoggedOut>
  <ifLoggedIn>
    <div class="page-header"><h1>Hello, <username/></h1></div>
  </ifLoggedIn>
</apply>
