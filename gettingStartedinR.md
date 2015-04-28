# Getting started in R #

## R settings ##

You need to have the library RODBC installed to access the database.
```
install.packages("RODBC",repos=getOption("repos"))```

Start a recent version of R, preferably R 2.14.x and load the appropriate libraries
```
library(RODBC)```
Copy one of the templates from the WIKI to your R editor.<br>
Example:<br>
<pre><code>SELECT * FROM vms WHERE<br>
rgn_local_date BETWEEN '01-jan-2010' and '31-jan-2010'</code></pre>
Make sure the template is 'pasted' together and written to a new variable.<br>
<pre><code>query &lt;- paste("SELECT * FROM vms WHERE<br>
rgn_local_date BETWEEN '01-jan-2010' and '31-jan-2010'")</code></pre>
The query is ready to be executed, but first a connection to the VISSTAT database needs to be established. To make a connection, follow the following line where user is the username similar to your Raja username and password is either a password you got from Peter van der Kamp:<br>
<pre><code>conn &lt;- odbcConnect(dsn="visstatp",uid=user,pwd=passwd,believeNRows=F)</code></pre>
Next step is to actually evaluate the query.<br>
<pre><code>visstatData &lt;- sqlQuery(conn,query)</code></pre>
Please note that some queries might take a considerable amount of time to run! If it's not going quick enough, consider to query VISSTAT directly from Raja (more advanced).