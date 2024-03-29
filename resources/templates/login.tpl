<apply template="default">
  <div class="page-header"><h1>Log in</h1></div>
  <div class="row">
    <auth-error>
      <div class="alert-message error">Username and/or password invalid.</div>
    </auth-error>
  </div>
  <div class="row">
    <form action="/login" method="POST" class="form-stacked">

      <fieldset>
	<div class="clearfix$(username-error)">
	  <label for="username">Username</label>
	  <div class="input">
	    <input name="username" id="username" type="text" value="$(username-value)" />
	    <username-errors><span class="help-inline"><error/></span></username-errors>
	  </div>
	</div>
	<div class="clearfix$(password-error)">
	  <label for="password">Password</label>
	  <div class="input">
	    <input name="password" id="password" type="password" />
	    <password-errors><span class="help-inline"><error/></span></password-errors>
	  </div>
	</div>
      </fieldset>
      <div class="actions">
	<button type="submit" class="btn primary">Log in</button>
      </div>
    </form>
  </div>
</apply>
