<apply template="default">
  <div class="page-header"><h1>Register</h1></div>
  <div class="row">
    <form action="/register" method="POST" class="form-stacked">
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
	<button type="submit" class="btn primary">Register</button>
      </div>
    </form>
  </div>
</apply>
