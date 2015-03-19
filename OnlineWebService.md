# Setup (php) #

I suppose you are installing the OnLine service on your PC Linux

You have to install a web server with php support. For php and linux (Debian and Ubuntu), for instance you simply run

```
  sudo apt-get install apache2-mpm-prefork libapache2-mod-php5
```

Be sure that the STRATEGICO\_HOME is set for the user or the script that starts Apache: for Debian/Ubuntu add it in /etc/apache2/envvars


The document root is the directory that corresponds to the url http://localhost
For Linux it is /var/www

You have to set the right paths in the file "strategico.config" (http://code.google.com/p/strategico/source/browse/trunk/strategico.config)

```
  projects.home="/var/www/strategico/projects",
  projects.url="http://localhost/strategico/projects",
```

In your document root, you should have at least the following directories and files (taken by strategico sources)

```
   strategico/projects/web-ltp
   strategico/projects/web-ltp/project.config
   strategico/web/ws.html
   strategico/web/ws.brew
   strategico/web/ws.php
```

Change:
  * in ws.html the string ws.brew with ws.php
  * in ws.php the path of strategico sources (by default /apps/strategico)

Be sure that the folder projects/web-ltp/1200 will be WRITABLE by the user that runs Apache. You can do

```
  cd /var/www/strategico
  mkdir projects/web-ltp/1200
  sudo chmod 777 projects/web-ltp/1200
```

# Setup (rapache/brew) #

## brew ##
In stall the R [brew](http://cran.r-project.org/web/packages/brew/index.html) package

## RApache ##

You need to download, install and configure [rapache](http://rapache.net/). For Debian and Ubuntu I suugest to create two files
  * /etc/apache2/mods-available/R.load
```
LoadModule R_module /usr/lib/apache2/modules/mod_R.so
```

  * etc/apache2/mods-available/R.conf
```
 <IfModule mod_R.c>
    ROutputErrors
    #REvalOnStartup "library(DBI); library(RMySQL)"
    #RSourceOnStartup "/var/www/lib/R/startup.R"
 </IfModule>
```

And then enable the module with
```
 a2enmod R
```

After that, add in your apache web server configuration, inside your virtualhost (file /etc/apache2/sites-enabled/000-default for Debian and Ubuntu)

```
   <Location /RApacheInfo>
        SetHandler r-info
   </Location>

   <Files *.brew>
        SetHandler r-script
        RHandler brew::brew
   </Files>
```

An example of configuration file could be

```
<VirtualHost *:80>
	ServerAdmin webmaster@localhost

	DocumentRoot /var/www
	<Directory />
		Options FollowSymLinks
		AllowOverride None
	</Directory>
	<Directory /var/www/>
		Options Indexes FollowSymLinks MultiViews
		AllowOverride None
		Order allow,deny
		allow from all
	</Directory>

	ErrorLog ${APACHE_LOG_DIR}/error.log
	LogLevel warn
	CustomLog ${APACHE_LOG_DIR}/access.log combined

    # Prints out a nice report about R running within Apache
   <Location /RApacheInfo>
	SetHandler r-info
   </Location>

   <Files *.brew>
        SetHandler r-script
        RHandler brew::brew
   </Files>

</VirtualHost>
```

In this case all files with extension .brew will be run with R & brew