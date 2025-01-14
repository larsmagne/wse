This is a Wordpress plugin for gathering visitor statistics, as well
as an Emacs package for displaying the information.

Installation Instructions
=========================

Find the Wordpress plugin directory.  It's usually something like

    /var/www/wp-content/plugins/
  
Create a directory called "wse" under that, and copy over all the
.php and .js files there.

Edit the wse/password.php file and insert a password that will be
used to poll the data.  Don't use the "real" Wordpress password, but
just something nice and long and secret.

Then, in your admin interface, go to the Plugins page and activate the
plugin.  That's all you need to do on the server side.

Emacs Usage
===========

The Emacs bit requires a newish Emacs (it uses SQLite support and
stuff), and also requires the eplot library, which can be found here:

    https://github.com/larsmagne/eplot
	
Then you need the following in your init file:

    (setq wse-blogs '("example.com" "zot.com"))
	
Or wherever your blogs live.

Then ideally you should be able to say "M-x wse" and everything
should work, but er I wouldn't get my hopes up until the code has been
cleaned up some more.

